module Ast where

data ComScr
  = Com FilePath [String]
  | Scr Script
  deriving (Show, Eq)

data Pattern
  = ExitCode Int
  | Var String
  | Wildcard
  deriving (Show, Eq)

data Case
  = Case Pattern ComScrCase
  deriving (Show, Eq)

data ComScrCase
  = ComScrCase ComScr [ComScr] [Case]
  deriving (Show, Eq)

newtype Script
  = Script [ComScrCase]
  deriving (Show, Eq)
