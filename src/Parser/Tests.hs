module Parser.Tests where

import Ast
import Parser.Impl
import Test.QuickCheck

import Control.Monad ( liftM2 )

nbsp :: [Char]
nbsp = " \t"

nbspd :: String -> Gen String
nbspd x = fmap (++ x) $ resize 2 $ listOf $ elements nbsp

nbspd1 :: String -> Gen String
nbspd1 x = fmap (++ x) $ resize 2 $ listOf1 $ elements nbsp

data PatternTC = PatternTC Pattern String
  deriving (Show, Eq)

instance Arbitrary PatternTC where
  arbitrary = exitCode
    where
      exitCode = do
        code <- elements [0..255]
        return $ PatternTC (ExitCode code) (show code)

prop_exitCode0 :: Bool
prop_exitCode0 = parse (pPattern) "0" == Right (ExitCode 0)

prop_exitCode1 :: Bool
prop_exitCode1 = parse (pPattern) "1" == Right (ExitCode 1)

prop_wildcard :: Bool
prop_wildcard = parse (pPattern) "_" == Right Wildcard

prop_pPattern :: PatternTC -> Bool
prop_pPattern (PatternTC expected s) = parse (pPattern) s == Right expected

data WordTC = WordTC String String
  deriving (Show, Eq)

instance Arbitrary WordTC where
  arbitrary = do
    word <- listOf1 (elements chars)
    ugly <- nbspd word
    return $ WordTC word ugly
    where
      chars = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-+_/."

  shrink (WordTC [] _) = []
  shrink (WordTC [_] _) = []
  shrink (WordTC word ugly) =
    let
      halfLength = length word `div` 2
      left = take halfLength word
      right = drop halfLength word
      middle = drop (length word `div` 4) $ take halfLength word
      prefix = take (length ugly - length word) ugly
    in
      map (\w -> WordTC w (prefix ++ w)) [left, middle, right]

prop_pWord :: WordTC -> Bool
prop_pWord (WordTC expected s) = parse (pWord) s == Right expected

data ComScrTC = ComTC ComScr String [WordTC]
              | ScrTC ComScr String [ComScrTC]
  deriving (Show, Eq)

{-

It is slighly ugly, but useful, that ComTC retains an *altered* list of
WordTCs, where nbspd1 is used in place of nbspd. The slightly more elegant
solution seems to be introduce an additional WordTC1 type for those WordTCs
which are generated using nbspd1, but that type would share e.g. the shrink
method with the original WordTC. Inheritance would've been useful here.

-}

{-
comsToScr :: [ComScrTC] -> ComScrTC
comsToScr comScrTCs =
  ScrTC (Scr (Script scrComs)) (concat uglys) comScrTCs
    where
      unpack (ScrTC e s _) = (e, s)
      unpack (ComTC e s _) = (e, s)
      (scrComs, uglys) = unzip $ map unpack comScrTCs
-}

wordsToCom :: [WordTC] -> ComScrTC
wordsToCom wordTCs =
  ComTC (Com prettys) (concat uglys) wordTCs
  where
    unpack (WordTC pretty ugly) = (pretty, ugly)
    (prettys, uglys) = unzip $ map unpack wordTCs

commandWords :: [WordTC] -> Gen [WordTC]
commandWords wordTCs =
  mapM fix wordTCs
  where
    fix (WordTC pretty _) = fmap (WordTC pretty) (nbspd1 pretty)


instance Arbitrary ComScrTC where

  arbitrary = com

    where

      com = do
        wordTCs <- listOf1 arbitrary
        wordTCs' <- commandWords wordTCs
        return $ wordsToCom wordTCs'
{-
      scr = do
        comScrTCs <- listOf1 arbitrary
        return $ comsToScr comScrTCs
-}

  shrink (ComTC _ _ []) = []
  shrink (ComTC _ _ [_]) = []
  shrink (ComTC _ _ words') =
    let
      halfLength = length words' `div` 2
      left = take halfLength words'
      right = drop halfLength words'
      middle = drop (length words' `div` 4) $ take halfLength words'
    in
      map wordsToCom [left, middle, right]
  shrink _ = []

prop_pComScr :: ComScrTC -> Bool
prop_pComScr comScrTC = parseAll (pComScr) s == Right e
  where
    unpack (ScrTC e' s' _) = (e', s')
    unpack (ComTC e' s' _) = (e', s')

    (e, s) = unpack comScrTC

data CaseTC = CaseTC Case String
  deriving (Show, Eq)

instance Arbitrary CaseTC where
  arbitrary = do
    (PatternTC pattern s1) <- arbitrary
    (ComScrCaseTC comScrCase s3) <- arbitrary
    let e = Case pattern comScrCase
    s1' <- nbspd s1
    s2' <- nbspd ")"
    s3' <- nbspd s3
    let s = s1' ++ s2' ++ s3'
    return $ CaseTC e s

data ComScrCaseTC = ComScrCaseTC ComScrCase String
  deriving (Show, Eq)

instance Arbitrary ComScrCaseTC where
  arbitrary = do
    (ComScrTC first s1) <- arbitrary
    rest <- listOf arbitrary
    cases <- listOf arbitrary -- pick from a bag instead
    return $ ComScrCase first rest cases

smallCheck :: Testable prop => prop -> IO ()
smallCheck = quickCheckWith stdArgs { maxSuccess = 10 }

main :: IO ()
main = do
  quickCheck prop_exitCode0
  quickCheck prop_exitCode1
  quickCheck prop_wildcard
  smallCheck prop_pPattern
  smallCheck prop_pWord
  smallCheck prop_pComScr
