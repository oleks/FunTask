module Parser.Impl where

import Control.Monad ( void )
import Data.Char ( isDigit, isSpace )

import Text.ParserCombinators.ReadP

import Ast

import Control.Applicative ( Alternative( (<|>) ) )

data ParseError a = NoParse String
                  | IncompleteParse (a, String)
                  | AmbiguousGrammar [(a, String)]
  deriving (Show, Eq)

-- Tokenization/spacing combinators

script :: ReadP open -> ReadP close -> ReadP p -> ReadP p
script open close p = between (open <* skipSpaces) (skipSpaces >> close) p

wordSpaces :: ReadP ()
--wordSpaces = void $ many (nbsp <|> string "\\\n")
wordSpaces = void $ nbsp
  where nbsp = munch (\ c -> c /= '\n' && isSpace c)

lineSpaces :: ReadP ()
lineSpaces = void $ skipSpaces

token :: ReadP a -> ReadP a
token p = wordSpaces >> p

skipToken :: ReadP a -> ReadP ()
skipToken p = void $ token p

skipString :: String -> ReadP ()
skipString = skipToken . string

fixedToken :: String -> token -> ReadP token
fixedToken s t = t <$ skipString s

eol :: ReadP ()
eol = skipToken $ char '\n' <* skipSpaces

eofSpaces :: ReadP a -> ReadP a
eofSpaces p = p <* (skipSpaces >> eof)

-- Parsing primitives

parse :: ReadP a -> String -> Either (ParseError a) a
parse p s = case (readP_to_S p) s of
  [(a, "")] -> Right a
  [] -> Left $ NoParse s
  [(a, r)] -> Left $ IncompleteParse (a, r)
  ps -> Left $ AmbiguousGrammar ps

parseAll :: ReadP a -> String -> Either (ParseError a) a
parseAll p s = parse (skipSpaces >> eofSpaces p) s

-- Simple grammar

pPattern :: ReadP Pattern
pPattern = pExitCode <|> pWildcard

pExitCode :: ReadP Pattern
pExitCode = do
  digits <- token (munch1 isDigit)
  let exitCode = (read digits)
  if length digits > 3
  then pfail
  else  if exitCode > 255
        then pfail
        else return $ ExitCode exitCode

pWildcard :: ReadP Pattern
pWildcard = fixedToken "_" Wildcard

pPipeline :: ReadP (Command, [Command])
pPipeline = do
  pipeline <- sepBy1 pCommand (skipString "|")
  return $ (head pipeline, tail pipeline)

pCommand :: ReadP Command
pCommand = pCom <|> pScr

pScr :: ReadP Command
pScr = fmap Scr $ script (skipString "{") (string "}") pScript

pCom :: ReadP Command
pCom = do
  filePath <- pWord
  cmdLineArgs <- many pWord
  return $ Com filePath cmdLineArgs

pWord :: ReadP String
pWord = token $ munch1 (`elem`
  (['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-+_/."))

pScript :: ReadP Script
pScript = fmap Script (sepBy pCommandCase eol)

pCommandCase :: ReadP CommandCase
pCommandCase = do
  (first, rest) <- pPipeline
  -- Don't do eol here, as the last command may not be terminated by a \n.
  cases <- option [] pCases
  return $ CommandCase first rest cases

pCases :: ReadP [Case]
pCases = (skipToken $ char '\n') >> many1 (pCase)

pCase :: ReadP Case
pCase = do
  pattern <- pPattern
  skipToken $ char ')'
  commandCase <- pCommandCase
  return $ Case pattern commandCase

parseString :: String -> Either (ParseError Script) Script
parseString = parseAll pScript

parseFile :: FilePath -> IO (Either (ParseError Script) Script)
parseFile path = fmap parseString $ readFile path
