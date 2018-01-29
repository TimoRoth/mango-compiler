{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Mango.Compiler.Error (
    -- Error Reporting
    CompilerT,
    compilerT,
    runCompilerT,
    mapCompilerT,
    sequentialC,
    parallelC,
    report,
    reportMany,
    stop,
    stopOnError,
    mapAccumM,
    -- Diagnostics
    Diagnostic (..),
    diagnosticPretty,
    diagnosticPretty',
    ) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.State (StateT (..), runStateT, get, put)
import Control.Monad.Trans.Writer.Strict (WriterT (..), runWriterT)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Bool
import Data.ByteString (ByteString)
import Data.Either (Either (..), partitionEithers)
import Data.Eq
import Data.Function
import Data.Functor.Identity
import Data.Int (Int)
import Data.List
import Data.Maybe (Maybe (..), maybe)
import Data.Ord
import Data.Semigroup
import Data.String (String)
import Data.Tuple
import Mango.Compiler.Syntax
import Prelude (Num (..), mod)
import Text.Megaparsec.Error (ErrorItem, showErrorComponent)
import Text.Megaparsec.Pos (unPos)
import Text.Show

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Set as E

--------------------------------------------------------------------------------

type CompilerT e m = MaybeT (WriterT [e] m)

compilerT :: (Monad m) => m (Either [e] a) -> CompilerT e m a
compilerT = MaybeT . WriterT . fmap go
    where
        go (Right x) = (Just x,  [])
        go (Left es) = (Nothing, es)

runCompilerT :: (Monad m) => CompilerT e m a -> m (Either [e] a)
runCompilerT = fmap go . runWriterT . runMaybeT
    where
        go (Just x, []) = Right x
        go (_,      es) = Left es

mapCompilerT :: (Monad m, Monad n) => (m (Either [e] a) -> n (Either [e'] b)) -> CompilerT e m a -> CompilerT e' n b
mapCompilerT f = compilerT . f . runCompilerT

sequentialC :: (Monad m) => [CompilerT e m a] -> CompilerT e m [a]
sequentialC = MaybeT . fmap sequence . mapM runMaybeT

parallelC :: (Monad m) => [CompilerT e Identity a] -> CompilerT e m [a]
parallelC = compilerT . return . eitherPartitions . partitionEithers . parMap rseq (runIdentity . runCompilerT)
    where
        eitherPartitions :: ([[a]], b) -> Either [a] b
        eitherPartitions ([], x) = Right x
        eitherPartitions (es, _) = Left (concat es)

report :: (Monad m) => e -> CompilerT e m ()
report = reportMany . (:[])

reportMany :: (Monad m) => [e] -> CompilerT e m ()
reportMany = MaybeT . WriterT . return . (,) (Just ())

stop :: (Monad m) => CompilerT e m a
stop = (MaybeT . WriterT . return . (,) Nothing) []

stopOnError :: (Monad m) => CompilerT e m a -> CompilerT e m a
stopOnError = mapCompilerT id

mapAccumM :: (Monad m) => (a -> b -> m (a, c)) -> a -> [b] -> m (a, [c])
mapAccumM f a l = fmap swap (runStateT (mapM go l) a)
    where
        go i = do
            s <- get
            (s', r) <- lift (f s i)
            put s'
            return r

--------------------------------------------------------------------------------

data Diagnostic
    = GenericError { diagnostic_location :: !Location, genericError_message :: String }
    | SyntaxError  { diagnostic_location :: !Location, syntaxError_unexpected :: Maybe (ErrorItem SyntaxToken), syntaxError_expecting :: E.Set (ErrorItem SyntaxToken) }
    | BindError    { diagnostic_location :: !Location, bindError_name :: String, bindError_candidates :: [String] }

diagnosticPretty :: Diagnostic -> String
diagnosticPretty diagnostic =
    show (diagnostic_location diagnostic) <> ":\n" <>
    errorMessage diagnostic

diagnosticPretty' :: Int -> Diagnostic -> String
diagnosticPretty' tabWidth diagnostic =
    show (diagnostic_location diagnostic) <> ":\n" <>
    linePretty tabWidth (location_sourceText (diagnostic_location diagnostic)) (location_sourcePos (diagnostic_location diagnostic)) <>
    errorMessage diagnostic

errorMessage :: Diagnostic -> String
errorMessage GenericError { genericError_message = message } =
    message ++ "\n"
errorMessage SyntaxError { syntaxError_unexpected = unexpected, syntaxError_expecting = expecting } =
    errorItemsPretty "unexpected " (maybe E.empty E.singleton unexpected) ++
    errorItemsPretty "expecting " expecting
errorMessage BindError { bindError_name = name, bindError_candidates = [] } =
    "\"" ++ name ++ "\" is not declared\n"
errorMessage BindError { bindError_name = name, bindError_candidates = candidates } = 
    "\"" ++ name ++ "\" is ambiguous between the following declarations:" ++ concatMap ("\n    " ++) candidates ++ "\n"

errorItemsPretty :: String -> E.Set (ErrorItem SyntaxToken) -> String
errorItemsPretty prefix ts
    | E.null ts = ""
    | otherwise = prefix <> orList (E.toAscList (E.map showErrorComponent ts)) <> "\n"

orList :: [String] -> String
orList []     = ""
orList [x]    = x
orList [x, y] = x <> " or " <> y
orList xs     = intercalate ", " (init xs) <> ", or " <> last xs

linePretty :: Int -> ByteString -> SourcePos -> String
linePretty w s p =
    padding    <> " |\n" <>
    lineNumber <> " | " <> rline <> "\n" <>
    padding    <> " | " <> rpadding <> "^\n"
    where
        line       = C.unpack (selectLine (unPos (sourceLine p) - 1) s)
        lineNumber = show (unPos (sourceLine p))
        padding    = replicate (length lineNumber) ' '
        rline      = expandTab w line
        rpadding   = expandPad w (take (unPos (sourceColumn p) - 1) line)

selectLine :: Int -> ByteString -> ByteString
selectLine l = go 0
    where
        go !n !s
            | n == l                                                   = s1
            | B.null s2                                                = B.empty
            | B.head s2 == 13 && B.length s2 > 1 && B.index s2 1 == 10 = go (n + 1) (B.drop 2 s2)
            | otherwise                                                = go (n + 1) (B.tail s2)
            where
                (s1, s2) = B.break (\x -> x == 13 || x == 10) s

expandTab :: Int -> String -> String
expandTab w = go 0
    where
        go  _ []        = []
        go !n ('\t':xs) = replicate (w - mod n w) ' ' <> go 0 xs
        go !n (   x:xs) = (if x < ' ' then ' ' else x):go (n + 1) xs

expandPad :: Int -> String -> String
expandPad w = go 0
    where
        go  _ []        = []
        go !n ('\t':xs) = replicate (w - mod n w) ' ' <> go 0 xs
        go !n (   _:xs) = ' ':go (n + 1) xs

--------------------------------------------------------------------------------
