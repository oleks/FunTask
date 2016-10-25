module Interp.Impl where

import Control.Monad ( void )
-- import Control.Monad.Trans.Except ( ExceptT, throwE )
import GHC.IO.Handle ( Handle )
import System.Exit ( ExitCode (..) )
import System.IO ( stdin, stdout, stderr )
import System.Process (
    ProcessHandle, StdStream (..), std_in, std_out, std_err,
    proc, createProcess, waitForProcess
  )

import Ast

data Error = NoCommands

createPipe :: [Command] -> (Handle, Handle, Handle) -> IO [ProcessHandle]
createPipe [] _ = return []
createPipe [(Com fPath args)] (hIn, hOut, hErr) = do
  (_, _, _, pHndl) <-
    createProcess (proc fPath args) {
        std_in = UseHandle hIn,
        std_out = UseHandle hOut,
        std_err = UseHandle hErr
      }
  return [pHndl]
createPipe ((Com fPath args) : pipe) (hIn, hOut, hErr) = do
  (_, Just hPipeOut, _, pHndl) <-
    createProcess (proc fPath args) {
        std_in = UseHandle hIn,
        std_out = CreatePipe,
        std_err = UseHandle hErr
      }
  fmap (pHndl :) $ createPipe pipe (hPipeOut, hOut, hErr)

interpPipe :: [Command] -> IO ExitCode
interpPipe pipe = do
  pHndls <- createPipe pipe (stdin, stdout, stderr)
  waitOrReturn pHndls
  where
    waitOrReturn [] = return ExitSuccess
    waitOrReturn [h] = waitForProcess h
    waitOrReturn (h:hs) = do
      exitCode <- waitForProcess h
      case exitCode of
        ExitSuccess -> waitOrReturn hs
        _ -> return exitCode

interpCases :: Int -> [Case] -> IO ExitCode
interpCases e ((Case (ExitCode c) commandCase):cs) =
  if e == c
  then interpCommandCase commandCase
  else interpCases e cs
interpCases _ ((Case (Wildcard) commandCase):_) =
  interpCommandCase commandCase
interpCases e [] = return $ ExitFailure e

interpCommandCase :: CommandCase -> IO ExitCode
interpCommandCase (CommandCase first rest cases) = do
  exitCode <- interpPipe (first:rest)
  case exitCode of
    ExitSuccess -> return ExitSuccess
    ExitFailure e -> interpCases e cases

interpCommandCases :: [CommandCase] -> IO ExitCode
interpCommandCases [] = return ExitSuccess
interpCommandCases (c:cs) = do
  exitCode <- interpCommandCase c
  case exitCode of
    ExitSuccess -> interpCommandCases cs
    _ -> return exitCode

runScript :: Script -> IO ()
runScript (Script commandCases) = void $ interpCommandCases commandCases
