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


-- Square root function
(^!) :: Num a => a -> Int -> a
(^!) x n = x^n

squareRoot :: Int -> Int
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
  in  head $ dropWhile (not . isRoot) iters


instance Arbitrary Expr where
    arbitrary = sized superArb
        where
            superArb :: Int -> Gen Expr
            superArb n = frequency [(5, commaArb n),
                                    (10, constArb n),
                                    (5, simpleOptArb n),
                                    (1, arrayArb n)]
            commaArb :: Int -> Gen Expr
            commaArb 0 = constArb 0
            commaArb n = do
                let n' = n `quot` 100
                l <- superArb n'
                r <- superArb n'
                return $ Comma l r
            constArb :: Int -> Gen Expr
            constArb size = do
                n <- choose(-99999999, 99999999)
                VI i <- resize size arbitrary
                s <- vectorOf size $ arbitrary `suchThat` (/= '\'')
                elements [Number n,
                          String s,
                          Undefined,
                          TrueConst,
                          FalseConst,
                          Var i]
            simpleOptArb :: Int -> Gen Expr
            simpleOptArb 0 = constArb 0
            simpleOptArb n = let n' = n `quot` 2 in
                do
                    e1 <- superArb n'
                    e2 <- superArb n'
                    opt <- elements operators
                    return $ Call opt [e1, e2]
            arrayArb :: Int -> Gen Expr
            arrayArb 0 = return $ Array []
            arrayArb n = let n' = squareRoot n in
                do
                exprs <- vectorOf n' (superArb n')
                return $ Array exprs

instance Arbitrary Stm where
    

instance Arbitrary Program where
    arbitrary = do
        expr <- arbitrary
        return $ Prog [ExprAsStm expr]



prettyPrintExpr  :: Expr -> String
prettyPrintExpr (Number n)    = show n
prettyPrintExpr (String s)    = '\'' : s ++ "'"
prettyPrintExpr Undefined     = "undefined"
prettyPrintExpr TrueConst     = "true"
prettyPrintExpr FalseConst    = "false"
prettyPrintExpr (Var s)       = s
prettyPrintExpr (Comma e1 e2) = prettyPrintExpr e1 ++ " , " ++ prettyPrintExpr e2
prettyPrintExpr (Call opt (a:as)) = if opt `elem` operators
    then prettyPrintExpr a ++ opt ++ prettyPrintExpr (head as)
    else opt ++ "(" ++ concatMap prettyPrintExpr (a:as) ++ ")"
prettyPrintExpr (Array exprs) = "[" ++ intercalate ", " (map prettyPrintExpr exprs) ++ "]"
prettyPrintExpr _ = undefined
