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

createPipe :: [ComScr] -> (Handle, Handle, Handle) -> IO [ProcessHandle]
createPipe [] _ = return []
createPipe [(Com fPath args)] (hin, hOut, hErr) = do
  (_, _, _, pHndl) <-
    createProcess (proc fPath args) {
        std_in = UseHandle hin,
        std_out = UseHandle hOut,
        std_err = UseHandle hErr
      }
  return [pHndl]
createPipe ((Com fPath args) : pipe) (hin, hOut, hErr) = do
  (_, Just hPipeOut, _, pHndl) <-
    createProcess (proc fPath args) {
        std_in = UseHandle hin,
        std_out = CreatePipe,
        std_err = UseHandle hErr
      }
  fmap (pHndl :) $ createPipe pipe (hPipeOut, hOut, hErr)

interpPipe :: [ComScr] -> IO ExitCode
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
interpCases e ((Case (ExitCode c) comScrCase):cs) =
  if e == c
  then interpComScrCase comScrCase
  else interpCases e cs
interpCases _ ((Case (Wildcard) comScrCase):_) =
  interpComScrCase comScrCase
interpCases e [] = return $ ExitFailure e

interpComScrCase :: ComScrCase -> IO ExitCode
interpComScrCase (ComScrCase first rest cases) = do
  exitCode <- interpPipe (first:rest)
  case exitCode of
    ExitSuccess -> return ExitSuccess
    ExitFailure e -> interpCases e cases

interpComScrCases :: [ComScrCase] -> IO ExitCode
interpComScrCases [] = return ExitSuccess
interpComScrCases (c:cs) = do
  exitCode <- interpComScrCase c
  case exitCode of
    ExitSuccess -> interpComScrCases cs
    _ -> return exitCode

runScript :: Script -> IO ()
runScript (Script comScrCases) = void $ interpComScrCases comScrCases
