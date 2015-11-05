{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module SubsParserTest where

import Data.Map( fromList )
import Test.HUnit

import SubsAst
import SubsInterpreter

-- Unit tests

-- Ident parser

introProg = Prog [
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

testInterpAdd = TestCase $
    assertEqual "Interp addition" (Right (fromList [("x", IntVal (-1))])) $
    runProg $ Prog [VarDecl "x" (Just (Call "+" [Number 1, Number (-2)]))]
testInterpAddNumberString = TestCase $
    assertEqual "Interp addition" (Right (fromList [("x", StringVal "1-2")])) $
    runProg $ Prog [VarDecl "x" (Just (Call "+" [Number 1, String "-2"]))]
testInterpAddStrings = TestCase $
    assertEqual "Interp addition" (Right (fromList [("x", StringVal "Hello, world!")])) $
    runProg $ Prog [VarDecl "x" (Just (Call "+" [String "Hello, ", String "world!"]))]

testInterpMult = TestCase $
    assertEqual "Interp multiplication" (Right (fromList [("x", IntVal 6)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "*" [Number 3, Number 2]))]

testInterpMod = TestCase $
    assertEqual "Interp modulo" (Right (fromList [("x", IntVal 1)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "%" [Number 5, Number 2]))]

testInterpSub = TestCase $
    assertEqual "Interp subtraction" (Right (fromList [("x", IntVal 3)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "-" [Number 5, Number 2]))]

testInterpLessTrue = TestCase $
    assertEqual "Interp less true" (Right (fromList [("x", TrueVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "<" [Number 2, Number 3]))]
testInterpLessFalse = TestCase $
    assertEqual "Interp less false" (Right (fromList [("x", FalseVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "<" [Number 3, Number 3]))]

testInterpLessTrueString = TestCase $
    assertEqual "Interp less string true" (Right (fromList [("x", TrueVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "<" [String "a", String "b"]))]
testInterpLessFalseString = TestCase $
    assertEqual "Interp less string false" (Right (fromList [("x", FalseVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "<" [String "a", String "a"]))]

testInterpEqTrue = TestCase $
    assertEqual "Interp eq true" (Right (fromList [("x", TrueVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "===" [Number 3, Number 3]))]
testInterpEqFalse = TestCase $
    assertEqual "Interp eq false" (Right (fromList [("x", FalseVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "===" [Number 3, Number 2]))]

testInterpEqTrueString = TestCase $
    assertEqual "Interp eq string true" (Right (fromList [("x", TrueVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "===" [String "a", String "a"]))]
testInterpEqFalseString = TestCase $
    assertEqual "Interp less string false" (Right (fromList [("x", FalseVal)])) $
    runProg $ Prog [VarDecl "x" (Just (Call "===" [String "a", String "b"]))]

testInterpArrayNew = TestCase $
    assertEqual "Interp Array.new()" (Right (fromList [("x", ArrayVal [UndefinedVal, UndefinedVal])])) $
    runProg $ Prog [VarDecl "x" (Just (Call "Array.new" [Number 2]))]


tests = TestList [
    testInterpAdd,
    testInterpAddNumberString,
    testInterpAddStrings,
    testInterpMult,
    testInterpMod,
    testInterpSub,
    testInterpLessTrue,
    testInterpLessFalse,
    testInterpLessTrueString,
    testInterpLessFalseString,
    testInterpEqTrue,
    testInterpEqFalse,
    testInterpEqTrueString,
    testInterpEqFalseString,
    testInterpArrayNew
    ]
