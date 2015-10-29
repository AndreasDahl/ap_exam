{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SubsParserTest where

import Control.Monad ( replicateM )
import Test.HUnit
import Test.QuickCheck
import Data.Char

import SimpleParse
import SubsParser
import SubsAst
import SubsAstArbitrary

-- Unit tests

-- Ident parser

testIdentParser = TestCase $
    assertEqual "for identParser \"foo\"," [("foo", [])] (parseEof identParser "foo")
testValidIdentWithOnlyDigits = TestCase $
    assertEqual "for identParser \"123\"," [] (parseEof identParser "123")
testValidIdentWithDigits = TestCase $
    assertEqual "for identParser \"a123\"," [("a123", [])] (parseEof identParser "a123")

testInvalidIdentDash = TestCase $
    assertEqual "for identParser \"a-b\"," [] $ parseEof identParser "a-b"

testInvalidIdentKeyword = TestCase $
    assertEqual "for identParser \"var\"," [] $ parseEof identParser "var"

-- Number parser

testGoodNumberParse = TestCase $
    assertEqual "for numberParser \"12345678\"" [(Number 12345678, [])] (parseEof numberParser "12345678")
testGoodNegativeNumberParse = TestCase $
    assertEqual "for numberParser \"-12345678\"" [(Number (-12345678), [])] (parseEof numberParser "-12345678")

testBadNegativeNumberFail = TestCase $
    assertEqual "for numberParser \"- 12345678\"" [] (parseEof numberParser "- 12345678")
testTooLongNumberFail = TestCase $
    assertEqual "for numberParser \"123456789\"" [] (parseEof numberParser "123456789")


tests = TestList [
    testIdentParser,
    testValidIdentWithOnlyDigits,
    testValidIdentWithDigits,
    testInvalidIdentKeyword,
    testGoodNumberParse,
    testGoodNegativeNumberParse,
    testBadNegativeNumberFail,
    testTooLongNumberFail]

-- QuickCheck

newtype InvalidIdent = II String
    deriving (Eq, Show)

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


-- Number Parser

newtype ValidNumber = VN String
    deriving (Eq, Show)

instance Arbitrary ValidNumber where
    arbitrary = do
        mi <- arbitrary
        len <- choose(1,8)
        n <- replicateM len (arbitrary `suchThat` isDigit)
        if mi then return $ VN $ '-' : n else return $ VN n

prop_ValidNumber (Spaces sp) (VN n) = parseEof numberParser (sp ++ n) == [(Number $ read n, [])]


-- Expr Parser

newtype ValidExpr = VE String
    deriving (Eq, Show)

-- instance Arbitrary ValidExpr where
--     arbitrary = do
--         VN n <- arbitrary
--         VI i <- arbitrary
--         Spaces s1 <- arbitrary
--         Spaces s2 <- arbitrary
--         elements [VE n,
--                   VE "true",
--                   VE "false",
--                   VE "undefined",
--                   VE $ n ++ ", true",
--                   VE $ n ++ s1 ++ "*" ++ s2 ++ n,
--                   VE $ n ++ s1 ++ "/" ++ s2 ++ n,
--                   VE $ n ++ s1 ++ "+" ++ s2 ++ n,
--                   VE $ n ++ s1 ++ "-" ++ s2 ++ n,
--                   VE i,
--                   VE $ i ++ s1 ++ "=" ++ s2 ++ n]

instance Arbitrary ValidExpr where
    arbitrary = do
        e <- arbitrary
        return $ VE (prettyPrintExpr e)


prop_ValidExpr (VE expr) = parseEof exprParser expr /= []

return []
runTests = $quickCheckAll
