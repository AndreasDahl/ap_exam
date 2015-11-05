module SubsAstArbitrary where

import Test.QuickCheck
import Data.Char
import Data.List

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

operators :: [String]
operators = ["*", "%", "+", "-", "<", "==="]

instance Arbitrary Expr where
    arbitrary = sized superArb
        where
            superArb :: Int -> Gen Expr
            superArb n = frequency [(10, constArb n),
                                    (5, simpleOptArb n),
                                    (5, assignArb n),
                                    (1, arrayComprArb n),
                                    (1, arrayArb n)]
            constArb :: Int -> Gen Expr
            constArb size = do
                n <- choose(-99999999, 99999999)
                VI i <- arbitrary
                s <- vectorOf size $ arbitrary `suchThat` (/= '\'')
                elements [Number n,
                          String s,
                          Undefined,
                          TrueConst,
                          FalseConst,
                          Var i]
            simpleOptArb :: Int -> Gen Expr
            simpleOptArb n | n <= 0 = constArb 0
            simpleOptArb n = let n' = n `quot` 2 in
                do
                    e1 <- superArb n'
                    e2 <- superArb n'
                    opt <- elements operators
                    return $ Call opt [e1, e2]
            arrayArb :: Int -> Gen Expr
            arrayArb n | n <= 0 = return $ Array []
            arrayArb n = let n' = floor (sqrt (fromIntegral n)) in
                do
                exprs <- vectorOf n' (superArb n')
                return $ Array exprs
            assignArb :: Int -> Gen Expr
            assignArb n = do
                VI i <- arbitrary
                expr <- superArb $ n-1
                return $ Assign i expr
            arrayComprArb :: Int -> Gen Expr
            arrayComprArb n = do
                VI i <- arbitrary
                expr1 <- superArb (n - 2)
                next <- arbitrary
                expr2 <- superArb (n - 2)
                return $ Compr (i, expr1, next) expr2

instance Arbitrary ArrayCompr where
    arbitrary = scale (`quot` 2) (oneof [forGen, ifGen])
        where
            forGen = do
                VI i <- arbitrary
                expr <- arbitrary
                next <- arbitrary
                return $ ArrayForCompr (i, expr, next)
            ifGen = do
                expr <- arbitrary
                next <- arbitrary
                return $ ArrayIf expr next

instance Arbitrary Stm where
    arbitrary = do
        VI i <- arbitrary
        expr <- arbitrary
        mbExpr <- arbitrary
        elements [ExprAsStm expr, VarDecl i mbExpr]


instance Arbitrary Program where
    arbitrary = do
        stms <- arbitrary
        return $ Prog stms

ppMbyCompr :: Maybe ArrayCompr -> String
ppMbyCompr Nothing = ""
ppMbyCompr (Just some) = prettyPrintArrayCompr some

prettyPrintArrayCompr :: ArrayCompr -> String
prettyPrintArrayCompr (ArrayForCompr (i, e, next)) =
    "for (" ++ i ++ " of " ++ prettyPrintExpr e ++ ") " ++ ppMbyCompr next
prettyPrintArrayCompr (ArrayIf e next) =
    "if (" ++ prettyPrintExpr e ++ ") " ++ ppMbyCompr next
    where

prettyPrintExpr  :: Expr -> String
prettyPrintExpr (Number n)    = show n
prettyPrintExpr (String s)    = '\'' : s ++ "'"
prettyPrintExpr Undefined     = "undefined"
prettyPrintExpr TrueConst     = "true"
prettyPrintExpr FalseConst    = "false"
prettyPrintExpr (Var s)       = s
prettyPrintExpr (Comma e1 e2) = prettyPrintExpr e1 ++ ", " ++ prettyPrintExpr e2
prettyPrintExpr (Call opt (a:as)) = if opt `elem` operators
    then prettyPrintExpr a ++ " " ++ opt ++ " " ++ prettyPrintExpr (head as)
    else opt ++ "(" ++ concatMap prettyPrintExpr (a:as) ++ ")"
prettyPrintExpr (Array exprs) = "[" ++ intercalate ", " (map prettyPrintExpr exprs) ++ "]"
prettyPrintExpr (Assign i expr) = i ++ " = " ++ prettyPrintExpr expr
prettyPrintExpr (Compr afor e) = "[ " ++ prettyPrintArrayCompr (ArrayForCompr afor) ++ prettyPrintExpr e ++ " ]"
prettyPrintExpr _ = undefined

ppStm :: Stm -> String
ppStm (ExprAsStm e) = prettyPrintExpr e
ppStm (VarDecl i mbyExpr) = "var " ++ i ++ " " ++ ppMbyExpr mbyExpr
    where
        ppMbyExpr Nothing = ""
        ppMbyExpr (Just e) = "= " ++ prettyPrintExpr e

ppProg :: Program -> String
ppProg (Prog stms) = foldl f "" stms
    where
        f s stm = s ++ ppStm stm ++ ";\n"
