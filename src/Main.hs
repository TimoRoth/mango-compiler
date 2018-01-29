{-# LANGUAGE NoImplicitPrelude #-}

module Main (
    main
    ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Bool
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.String
import Mango.Compiler.CodeGen
import Mango.Compiler.Emitter
import Mango.Compiler.Error
import Mango.Compiler.Parser
import Mango.Compiler.Syntax
import Mango.Compiler.Verifier
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (IO, FilePath, putStr, putStrLn)

data Options = Options { options_outputFile :: !(Maybe String), options_defaultOutputFile :: !String, options_asC :: !Bool, options_asDump :: !Bool, options_help :: !Bool }

defaultOptions :: Options
defaultOptions = Options Nothing "a.package" False False False

optDescrs :: [OptDescr (Options -> Options)]
optDescrs = [
    Option ['o'] [      ] (ReqArg (\f o -> o { options_outputFile = Just f                                                     }) "<file>") "write output to <file>",
    Option ['c'] [      ] (NoArg  (\  o -> o { options_defaultOutputFile = "a.inc", options_asC = True, options_asDump = False })         ) "emit modules as C code",
    Option [   ] ["dump"] (NoArg  (\  o -> o { options_defaultOutputFile = "a.txt", options_asDump = True, options_asC = False })         ) "emit modules as text dump",
    Option [   ] ["help"] (NoArg  (\  o -> o { options_help = True                                                             })         ) "display this help and exit"]

compiler :: (Options, [FilePath]) -> CompilerT Diagnostic IO ()
compiler (options, paths) = do
    syntaxTrees <- parseFiles paths
    verified <- verify (Compilation syntaxTrees)
    package <- compile verified
    let outputPath = fromMaybe (options_defaultOutputFile options) (options_outputFile options)
    if options_asC options then
        liftIO (emitPackageAsC outputPath package)
    else if options_asDump options then
        liftIO (dumpPackage outputPath package)
    else
        liftIO (emitPackage outputPath package)

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute optDescrs args of
        (options, paths, []) -> do
            let options' = foldl (flip id) defaultOptions options
            if options_help options' then do
                putStrLn (usageInfo header optDescrs)
            else do
                result <- runCompilerT (compiler (options', paths))
                case result of
                    Left diagnostics -> do
                        mapM_ (putStr . ("\n" ++) . diagnosticPretty' 4) diagnostics
                        exitFailure
                    Right () -> do
                        exitSuccess
        (_, _, errors) -> do
            putStr (concat errors)
            putStrLn (usageInfo header optDescrs)
    where
        header = "Usage: mango [options] <inputs>"
