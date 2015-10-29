module SubsAstArbitrary where

import Test.QuickCheck
import Data.Char

import SubsAst



newtype Spaces = Spaces String
    deriving (Eq, Show)

newtype ValidIdent = VI String
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

operators = ["*", "/", "+", "-", "<", "==="]


instance Arbitrary Expr where
    arbitrary = sized superArb
        where
            superArb :: Int -> Gen Expr
            superArb n = oneof [commaArb n, constArb, simpleOptArb n]
            commaArb :: Int -> Gen Expr
            commaArb 0 = constArb
            commaArb n = do
                let n' = n `quot` 2
                l <- superArb n'
                r <- superArb n'
                return $ Comma l r
            constArb :: Gen Expr
            constArb = do
                n <- choose(-99999999, 99999999)
                VI i <- arbitrary
                s <- listOf $ arbitrary `suchThat` (/= '"')
                elements [Number n,
                          String s,
                          Undefined,
                          TrueConst,
                          FalseConst,
                          Var i]
            simpleOptArb :: Int -> Gen Expr
            simpleOptArb n = let n' = n `quot` 2 in
                do
                    e1 <- superArb n'
                    e2 <- superArb n'
                    opt <- elements operators
                    return $ Call opt [e1, e2]



prettyPrintExpr  :: Expr -> String
prettyPrintExpr (Number n)    = show n
prettyPrintExpr (String s)    = '"' : s ++ "\""
prettyPrintExpr Undefined     = "undefined"
prettyPrintExpr TrueConst     = "true"
prettyPrintExpr FalseConst    = "false"
prettyPrintExpr (Var s)       = s
prettyPrintExpr (Comma e1 e2) = prettyPrintExpr e1 ++ " , " ++ prettyPrintExpr e2
prettyPrintExpr (Call opt (a:as)) = if opt `elem` operators
    then prettyPrintExpr a ++ opt ++ prettyPrintExpr (head as)
    else opt ++ "(" ++ concatMap prettyPrintExpr (a:as) ++ ")"
prettyPrintExpr _ = undefined
