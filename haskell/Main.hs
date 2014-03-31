module Main where

import Parser
import Codegen
import Emit
import JIT

import Data.Bool

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.General.AST as AST

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      ->    readExecutePrintLoop
    [filename] -> compileFile filename

readExecutePrintLoop :: IO ()
readExecutePrintLoop = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "proto> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ parseAndProcess mod REPL input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

parseAndProcess :: AST.Module -> CodegenMode -> String -> IO (Maybe AST.Module)
parseAndProcess astModule mode source = do
  let parseResult = parseToplevel source
  case parseResult of
    Left error -> hPrint stderr error >> return Nothing
    Right expressions -> do
      ast <- codegen astModule mode expressions
      return $ Just ast

compileFile :: String -> IO ()
compileFile filename = do
  filecontents <- readFile filename
  parseAndProcess initModule GenerateLLVM filecontents
  return ()
