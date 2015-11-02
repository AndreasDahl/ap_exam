module SubsInterpreter
    --    ( runProg
    --    , Error (..)
    --    , Value(..)
    --    )
       where

import SubsAst

-- You might need the following imports
import Control.Monad
import qualified Data.Map as Map
import Data.Map(Map)


-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)


initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", eqOp)
                       , ("<", lessOp)
                       , ("+", plusOp)
                       , ("*", mulOp)
                       , ("-", minusOp)
                       , ("%", divOp)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}


instance Functor SubsM where
  fmap = liftM

instance Applicative SubsM where
  pure = return
  fm <*> xm = do f <- fm
                 c <- xm
                 return $ f c

instance Monad SubsM where
  return x = SubsM $ \context -> Right (x, fst context)
  f >>= m = SubsM $ \c@(_, pe) -> do (v1, e1) <- runSubsM f c
                                     (v2, e2) <- runSubsM (m v1) (e1, pe)
                                     return (v2, e2)
  fail s = SubsM $ \ _ -> Left $ Error s


eqOp :: Primitive
eqOp [a, b] = if a == b then return TrueVal else return FalseVal
eqOp _ = fail "'===' called with non-number arguments"

lessOp :: Primitive
lessOp [IntVal a, IntVal b] = if a < b then return TrueVal else return FalseVal
lessOp [StringVal a, StringVal b] = if a < b then return TrueVal else return FalseVal
lessOp _ = fail "'<' called with non-number arguments"

mulOp :: Primitive
mulOp [IntVal a, IntVal b] = return $ IntVal (a * b)
mulOp _ = fail "'*' called with non-number arguments"

divOp :: Primitive
divOp [IntVal a, IntVal b] = return $ IntVal (a `mod` b)
divOp _ = fail "'%' called with non-number arguments"

plusOp :: Primitive
plusOp [IntVal a, IntVal b] = return $ IntVal (a + b)
plusOp [StringVal a, IntVal b] = return $ StringVal (a ++ show b)
plusOp [IntVal a, StringVal b] = return $ StringVal (show a ++ b)
plusOp _ = fail "'+' called with non-number arguments"

minusOp :: Primitive
minusOp [IntVal a, IntVal b] = return $ IntVal (a - b)
minusOp _ = fail "'-' called with non-number arguments"

arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(replicate n UndefinedVal)
arrayNew _ = fail "Array.new called with wrong number of arguments"


modify :: (Env -> Env) -> SubsM ()
modify f = SubsM modify'
    where modify' c = Right ((), f (fst c))

updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = modify $ Map.insert name val


getEnv :: SubsM Env
getEnv = SubsM getEnv'
    where getEnv' c = Right (fst c, fst c)


getVar :: Ident -> SubsM Value
getVar name = SubsM getVar'
    where getVar' c = case Map.lookup name (fst c) of
            Just v  -> Right (v, fst c)
            Nothing -> fail $ "Variable '" ++ name ++ "' not in scope"


getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM getFunction'
    where getFunction' c = case Map.lookup name (snd c) of
            Just f  -> Right (f, fst c)
            Nothing -> fail $ "Function '" ++ name ++ "' not in scope"


evalExpr :: Expr -> SubsM Value
evalExpr (Number n)   = return $ IntVal n
evalExpr (String s)   = return $ StringVal s
evalExpr (Array es)   = do { values <- mapM evalExpr es; return $ ArrayVal values }
evalExpr Undefined    = return UndefinedVal
evalExpr TrueConst    = return TrueVal
evalExpr FalseConst   = return FalseVal
evalExpr (Var i)      = getVar i
evalExpr (Assign i e) = do
    v <- evalExpr e
    updateEnv i v >> return v
evalExpr (Call name es) = do
    f <- getFunction name
    args <- mapM evalExpr es
    f args
evalExpr (Comma l r)    = evalExpr l >> evalExpr r
evalExpr (Compr afor e) = do
    oldEnv <- getEnv
    result <- evalArrayCompr (Just (ArrayForCompr afor)) e
    _      <- modify (const oldEnv)  -- Restore old Env
    return $ ArrayVal result
        where
            evalArrayCompr :: Maybe ArrayCompr -> Expr -> SubsM [Value]
            evalArrayCompr Nothing e = sequence [evalExpr e]
            evalArrayCompr (Just (ArrayForCompr (i, arrayE, more))) e = do
                array <- evalExpr arrayE
                case array of
                    ArrayVal vs -> liftM concat $
                        mapM (\value -> updateEnv i value >> evalArrayCompr more e) vs
                    StringVal s -> liftM concat $
                        mapM (\char -> updateEnv i (StringVal [char]) >> evalArrayCompr more e) s
                    _ -> fail $ "Expression '" ++ show arrayE ++ "' should be an array or a string"
            evalArrayCompr (Just (ArrayIf filt more)) e = do
                result <- evalExpr filt
                case result of
                    TrueVal  -> evalArrayCompr more e
                    FalseVal -> return []
                    _        -> fail "if statement should evaluate to a boolean"


stm :: Stm -> SubsM ()
stm (ExprAsStm e)       = void $ evalExpr e
stm (VarDecl i Nothing) = updateEnv i UndefinedVal
stm (VarDecl i (Just e))  = do { val <- evalExpr e; updateEnv i val}

program :: Program -> SubsM ()
program (Prog prog) = void $ mapM stm prog

runProg :: Program -> Either Error Env
runProg prog = case runSubsM (program prog) initialContext of
    Right ((), e) -> Right e
    Left err      -> Left err
