module Ast where

data Command
  = Com FilePath [String]
  | Scr Script
  deriving (Show, Eq)

data Pattern
  = ExitCode Int
  | Var String
  | Wildcard
  deriving (Show, Eq)

data Case
  = Case Pattern CommandCase
  deriving (Show, Eq)

data CommandCase
  = CommandCase Command [Command] [Case]
  deriving (Show, Eq)

newtype Script
  = Script [CommandCase]
  deriving (Show, Eq)
