{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Mango.Compiler.Lexer (
    tokenizeText,
    tokenizeBytes,
    tokenizeFile,
    tokenizeFiles,
    ) where

import Control.Applicative (Applicative (..))
import Control.Monad (Monad (..), ap, mapM)
import Data.Bool
import Data.ByteString
import Data.Eq
import Data.Function
import Data.Functor
import Data.Int (Int)
import Data.Maybe (Maybe (..))
import Data.Ord
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Mango.Compiler.Syntax
import Prelude (Num (..))
import System.IO (IO, FilePath)

import qualified Data.Set as E

--------------------------------------------------------------------------------

newtype Lexer a
    = Lexer { runLexer :: (ByteString, Location) -> (a, ByteString, Location) }

instance Functor Lexer where
    fmap f x = x >>= (pure . f)

instance Applicative Lexer where
    pure = return
    (<*>) = ap

instance Monad Lexer where
    return a = Lexer (\(b, l) -> (a, b, l))
    m >>= k  = Lexer (\s -> let (a, b', l') = runLexer m s in runLexer (k a) (b', l'))

at :: Lexer Location
at = Lexer (\(b, l) -> (l, b, l))

eol :: Lexer ()
eol = Lexer (\(b, Location path line _ text) -> ((), b, Location path (line + 1) 1 text))

eat :: (ByteString -> (a, ByteString)) -> Lexer a
eat f = Lexer (\(b, Location path line column text) -> let (x, b') = f b in (x, b', Location path line (column + (length b - length b')) text))

set :: (ByteString -> ByteString) -> Lexer ()
set f = eat (\b -> ((), f b))

get :: (ByteString -> a) -> Lexer a
get f = Lexer (\(b, l) -> (f b, b, l))

peek :: Int -> Lexer (Maybe Word8)
peek i = get (\b -> if i < length b then Just (index b i) else Nothing)

run :: Lexer a -> Lexer (a, ByteString)
run m = Lexer (\(b, l) -> let (x, b', l') = runLexer m (b, l) in ((x, take (length b - length b') b), b', l'))

--------------------------------------------------------------------------------

keywords :: E.Set ByteString
keywords = E.fromList [
    "bool", "i8", "u8", "i16", "u16", "i32", "u32", "i64", "u64", "f32" , "f64", "void",
    "module", "import", "type", "field", "declare", "define", "local",
    "nop", "break", "pop", "dup",
    "newobj", "newarr", "ldlen",
    "call", "calli", "syscall", "ret",
    "br", "brfalse", "brtrue", "beq", "bge", "bgt", "ble", "blt", "bne",
    "ldc", "ldftn", "ldnull",
    "ldarg", "ldarga", "starg",
    "ldloc", "ldloca", "stloc",
    "ldind", "stind",
    "ldfld", "ldflda", "stfld",
    "ldelem", "ldelema", "stelem",
    "add", "div", "mul", "rem", "sub", "neg",
    "and", "or", "xor", "shl", "shr", "not",
    "ceq", "cge", "cgt", "cle", "clt", "cne",
    "conv",
    "_", "$", "%", "@", "null", "true", "false"]

--------------------------------------------------------------------------------

scanMultiLineComment :: Lexer Bool
scanMultiLineComment =
    set (drop 2) >> go
    where
        go = do
            set (dropWhile (\x -> not (x == 42 || x == 13 || x == 10)))
            eof <- get null
            if eof then
                return False
            else do
                next <- get head
                case next of
                    42 -> do
                        next' <- peek 1
                        case next' of
                            Just 47 -> set (drop 2) >> return True
                            _       -> set (drop 1) >> go
                    13 -> do
                        next' <- peek 1
                        case next' of
                            Just 10 -> set (drop 2)
                            _       -> set (drop 1)
                        eol
                        go
                    10 -> do
                        set (drop 1)
                        eol
                        go
                    _ -> do
                        set (drop 1)
                        go

scanTrivia :: Bool -> Lexer [SyntaxTrivia]
scanTrivia isTrailing = do
    eof <- get null
    if eof then
        return []
    else do
        next <- get head
        case next of
            32 -> do
                value <- eat (span isWhiteSpace)
                rest <- scanTrivia isTrailing
                return (WhitespaceTrivia value:rest)
            13 -> do
                next' <- peek 1
                value <- case next' of
                    Just 10 -> eat (splitAt 2)
                    _       -> eat (splitAt 1)
                eol
                rest <- if isTrailing then pure [] else scanTrivia isTrailing
                return (EndOfLineTrivia value:rest)
            10 -> do
                value <- eat (splitAt 1)
                eol
                rest <- if isTrailing then pure [] else scanTrivia isTrailing
                return (EndOfLineTrivia value:rest)
            47 -> do
                next' <- peek 1
                case next' of
                    Just 47 -> do
                        value <- eat (break (\x -> x == 13 || x == 10))
                        rest <- scanTrivia isTrailing
                        return (SingleLineCommentTrivia value:rest)
                    Just 42 -> do
                        (success, value) <- run scanMultiLineComment
                        rest <- scanTrivia isTrailing
                        if success then
                            return (MultiLineCommentTrivia value:rest)
                        else
                            return (ErrorTrivia value:rest)
                    _ -> return []
            x | isWhiteSpace x -> do
                value <- eat (span isWhiteSpace)
                rest <- scanTrivia isTrailing
                return (WhitespaceTrivia value:rest)
              | otherwise -> do
                return []
    where
        isWhiteSpace x = (x == 32) || (x == 9) || (x == 11) || (x == 12)

scanIdentifierOrKeyword :: [SyntaxTrivia] -> Lexer SyntaxToken
scanIdentifierOrKeyword leadingTrivia = do
    pos <- at
    end <- get (\s -> 1 + length (takeWhile isIdContinue (tail s)))
    identifier <- get (take end)
    if E.member identifier keywords then do
        next <- peek end
        end' <- case next of
            Just 46 -> get (\s -> end + length (takeWhile isIdContinue' (drop end s)))
            _       -> return end
        keyword <- eat (splitAt end')
        trailingTrivia <- scanTrivia True
        return (KeywordToken leadingTrivia keyword trailingTrivia pos)
    else do
        set (drop end)
        trailingTrivia <- scanTrivia True
        return (IdentifierToken leadingTrivia identifier trailingTrivia pos)
    where
        isIdContinue  x = (x >= 65) && (x <= 90) || (x >= 97) && (x <= 122) || (x >= 48) && (x <= 57) || (x == 95)
        isIdContinue' x = (isIdContinue x) || (x == 46)

scanNumericLiteral :: [SyntaxTrivia] -> Lexer SyntaxToken
scanNumericLiteral leadingTrivia = do
    pos <- at
    literal <- eat (span isDigit)
    trailingTrivia <- scanTrivia True
    return (NumericLiteralToken leadingTrivia literal trailingTrivia pos)
    where
        isDigit x = (x >= 48) && (x <= 57)

scanPunctuator :: [SyntaxTrivia] -> Lexer SyntaxToken
scanPunctuator leadingTrivia = do
    pos <- at
    punctuator <- eat (splitAt 1)
    trailingTrivia <- scanTrivia True
    return (PunctuatorToken leadingTrivia punctuator trailingTrivia pos)

scanBad :: [SyntaxTrivia] -> Lexer SyntaxToken
scanBad leadingTrivia = do
    pos <- at
    character <- eat (splitAt 1)
    trailingTrivia <- scanTrivia True
    return (BadToken leadingTrivia character trailingTrivia pos)

scanToken :: Lexer SyntaxToken
scanToken = do
    leadingTrivia <- scanTrivia False
    eof <- get null
    if eof then do
        pos <- at
        return (EndOfFileToken leadingTrivia empty [] pos)
    else do
        next <- get head
        case next of
            33              -> scanPunctuator          leadingTrivia
            35              -> scanPunctuator          leadingTrivia
            38              -> scanPunctuator          leadingTrivia
            40              -> scanPunctuator          leadingTrivia
            41              -> scanPunctuator          leadingTrivia
            42              -> scanPunctuator          leadingTrivia
            43              -> scanPunctuator          leadingTrivia
            44              -> scanPunctuator          leadingTrivia
            45              -> scanPunctuator          leadingTrivia
            46              -> scanPunctuator          leadingTrivia
            47              -> scanPunctuator          leadingTrivia
            58              -> scanPunctuator          leadingTrivia
            59              -> scanPunctuator          leadingTrivia
            60              -> scanPunctuator          leadingTrivia
            61              -> scanPunctuator          leadingTrivia
            62              -> scanPunctuator          leadingTrivia
            63              -> scanPunctuator          leadingTrivia
            91              -> scanPunctuator          leadingTrivia
            92              -> scanPunctuator          leadingTrivia
            93              -> scanPunctuator          leadingTrivia
            94              -> scanPunctuator          leadingTrivia
            96              -> scanPunctuator          leadingTrivia
            123             -> scanPunctuator          leadingTrivia
            124             -> scanPunctuator          leadingTrivia
            125             -> scanPunctuator          leadingTrivia
            126             -> scanPunctuator          leadingTrivia
            x | isIdStart x -> scanIdentifierOrKeyword leadingTrivia
              | isDigit x   -> scanNumericLiteral      leadingTrivia
              | otherwise   -> scanBad                 leadingTrivia
    where
        isIdStart x = (x >= 65) && (x <= 90) || (x >= 97) && (x <= 122) || (x == 95) || (x == 36) || (x == 37) || (x == 64)
        isDigit x = (x >= 48) && (x <= 57)

scanTokens :: Lexer [SyntaxToken]
scanTokens = do
    token <- scanToken
    case token of
        EndOfFileToken {} -> do
            return [token]
        _ -> do
            tokens <- scanTokens
            return (token:tokens)

--------------------------------------------------------------------------------

tokenizeText :: FilePath -> Text -> [SyntaxToken]
tokenizeText path text =
    tokenizeBytes path (encodeUtf8 text)

tokenizeBytes :: FilePath -> ByteString -> [SyntaxToken]
tokenizeBytes path text =
    tokens
    where
        (tokens, _, _) = runLexer scanTokens (text, Location path 1 1 text)

tokenizeFile :: FilePath -> IO [SyntaxToken]
tokenizeFile path =
    tokenizeBytes path <$> readFile path

tokenizeFiles :: [FilePath] -> IO [[SyntaxToken]]
tokenizeFiles paths =
    mapM tokenizeFile paths

--------------------------------------------------------------------------------
