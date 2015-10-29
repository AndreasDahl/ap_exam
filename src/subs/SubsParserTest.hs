{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SubsParserTest where

import Test.HUnit
import Test.QuickCheck
import Data.Char

import SimpleParse
import SubsParser

-- Unit tests

testIdentParser = TestCase $
    assertEqual "for integerParser \"foo\"," [("foo", [])] (parseEof identParser "foo")
testValidIdentWithOnlyDigits = TestCase $
    assertEqual "for identParser \"123\"," [] (parseEof identParser "123")
testValidIdentWithDigits = TestCase $
    assertEqual "for identParser \"a123\"," [("a123", [])] (parseEof identParser "a123")

testInvalidIdentDash = TestCase $
    assertEqual "for identParser \"a-b\"," [] $ parseEof identParser "a-b"

testInvalidIdentKeyword = TestCase $
    assertEqual "for identParser \"var\"," [] $ parseEof identParser "var"

tests = TestList [
    testIdentParser,
    testValidIdentWithOnlyDigits,
    testValidIdentWithDigits,
    testInvalidIdentKeyword]

-- QuickCheck

newtype Spaces = Spaces String
    deriving (Eq, Show)

newtype ValidIdent = VI String
    deriving (Eq, Show)

newtype InvalidIdent = II String
    deriving (Eq, Show)

instance Arbitrary Spaces where
    arbitrary = do
        s <- listOf $ arbitrary `suchThat` isSpace
        return $ Spaces s

instance Arbitrary ValidIdent where
    arbitrary = do
        firstC <- firstCharGen
        str <- stringGen
        return $ VI $ firstC : str
        where

            firstCharGen :: Gen Char
            firstCharGen = arbitrary `suchThat` (\ c -> isLetter c || c == '_')
            stringGen :: Gen String
            stringGen = listOf $ arbitrary `suchThat` (\ c -> isDigit c || isLetter c || c == '_')

-- Can be extended to provide more invalid cases
instance Arbitrary InvalidIdent where
    arbitrary = do
        firstC <- invalidFirstChar
        str <- validTailGen
        return $ II $ firstC : str
        where
            invalidFirstChar :: Gen Char
            invalidFirstChar = arbitrary `suchThat` (\ c -> not (isSpace c || isLetter c || c == '_'))
            validTailGen :: Gen String
            validTailGen = listOf $ arbitrary `suchThat` (\ c -> isDigit c || isLetter c || c == '_')


prop_ValidIdent (Spaces sp)(VI s) = parseEof identParser (sp ++ s) == [(s, [])]

prop_InvalidIDent (II s) = null $ parseEof identParser s

return []
runTests = $quickCheckAll
