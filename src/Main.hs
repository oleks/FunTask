module Main where

import Ast
import Parser ( parseString )
import Interp

import System.Console.Haskeline

import System.Environment ( getArgs )
import System.Exit ( exitFailure )

import Data.List ( intercalate )

import Control.Monad.Trans.Class ( lift )

parseScript :: String -> IO Script
parseScript s =
  case parseString s of
    Left e -> error $ show e
    Right script -> return script

parseScriptPath :: FilePath -> IO Script
parseScriptPath path = readFile path >>= parseScript

runScriptPath :: FilePath -> IO ()
runScriptPath path = parseScriptPath path >>= runScript

-- Read/Eval/Print-Loop
repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "~> "
      case minput of
        Nothing -> return ()
        Just "exit" -> return ()
        Just input -> do
          lift $ parseScript input >>= runScript
          loop

showUsage :: IO ()
showUsage = do
  putStrLn "Usage: sh [[-c command]|<path>]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> repl
    [path] -> runScriptPath path
    ("-c":command) -> parseScript (intercalate " " command) >>= runScript
    _ -> do
      showUsage
      exitFailure
