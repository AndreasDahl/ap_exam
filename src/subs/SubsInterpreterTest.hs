{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE TemplateHaskell #-}
module SubsParserTest where

import Control.Monad ( replicateM )
import Test.HUnit
import Test.QuickCheck
import Data.Char

import SubsAst
import SubsInterpreter
import SubsAstArbitrary

-- Unit tests

-- Ident parser

testProg = Prog [
    VarDecl "xs" (
        Just (Array [
            Number 0, Number 1, Number 2,
            Number 3, Number 4, Number 5,
            Number 6, Number 7, Number 8, Number 9])),
    VarDecl "squares" (
        Just (Compr ("x",Var "xs", Nothing)
            (Call "*" [Var "x",Var "x"]))),
    VarDecl "evens" (
        Just (Compr ("x",Var "xs",
            Just (ArrayIf (
                Call "===" [
                    Call "%" [Var "x",Number 2], Number 0]) Nothing))
                (Var "x"))),
    VarDecl "many_a" (
        Just (Compr ("x",Var "xs",
            Just (ArrayForCompr ("y",Var "xs", Nothing)))
                (String "a"))),
    VarDecl "hundred" (
        Just (Compr ("i",Array [Number 0],
            Just (ArrayForCompr ("x",Var "xs",
                Just (ArrayForCompr ("y",Var "xs", Nothing)))))
                    (Assign "i" (Call "+" [Var "i", Number 1]))))
    ]

scopeProg = Prog [
    VarDecl "x" (Just (Number 42)),
    VarDecl "y" (Just (Compr ("k", String "abc", Nothing) (Var "k"))),
    VarDecl "z" (Just (Var "x"))
    ]


-- testInterpProg1 = TestCase $
--     (runProg testProg1)
--
-- tests = TestList [
--     testInterpProg1]

prop_expr :: Program -> Property
prop_expr prog
 = case runProg prog of
    Right _     -> counterexample "good" True
    Left (Error reason) -> counterexample reason False


return []
runTests = $quickCheckAll
