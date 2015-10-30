module SubsParser
{-(
  ParseError(..),
  parseString,
  parseFile
)
-}
 where


{----- GRAMMAR -----

Program ::= Stms

Stms ::= ϵ
      |  Stm `;` Stms

Stm  ::= `var` Ident AssignOpt
      |  Expr

AssignOpt  ::= ϵ
            |  `=` Expr

Expr       ::= Expr1 `,` Expr
            |  Expr1

Expr1      ::= Expr2
            |  Expr2 `+` Expr1
            |  Expr2 `-` Expr1
            |  Expr2 `*` Expr1
            |  Expr2 `%` Expr1
            |  Expr2 `<` Expr1
            |  Expr2 `===` Expr1

Expr2     ::=  Number
            |  String
            |  `true`
            |  `false`
            |  `undefined`
            |  Ident AfterIdent
            |  `[` Exprs `]`
            |  `[` `for` `(` Ident `of` Expr `)` ArrayCompr Expr `]`
            |  `(` Expr `)`


AfterIdent ::= ϵ
            |  `=` Expr1
            |  FunCall

FunCall    ::= `.` Ident FunCall
            |  `(` Exprs `)`

Exprs      ::= ϵ
            |  Expr1 CommaExprs

CommaExprs ::= ϵ
            |  `,` Expr1 CommaExprs

ArrayCompr  ::= ϵ
             |  `if` `(` Expr `)` ArrayCompr
             |  `for` `(` Ident `of` Expr `)` ArrayCompr

-----}

import Control.Applicative ( Alternative((<|>)) )
import Data.Char

import SubsAst
import SimpleParse


data ParseError = ParseError String
                deriving (Show, Eq)


keywords :: [String]
keywords = ["var", "true", "false", "undefined", "for", "of", "if"]

identParser :: Parser Ident
identParser = do
    s <- token $ munch1 $ \ c -> isLetter c || c == '_' || isDigit c
    if firstIsDigit s || s `elem` keywords then reject else return s
    where
        firstIsDigit (c:_) = isDigit c
        firstIsDigit []    = False


numberParser :: Parser Expr
numberParser = do
    _ <- spaces
    neg <- option $ char '-'
    n <- munch1 isDigit
    let number = read n in  -- Not completely safe as read may crash
        if number > 99999999 then fail "Too many digits in number"
                        else
                            case neg of
                                Just _  -> return $ Number $ -number
                                Nothing -> return $ Number number


exprsParser :: Parser [Expr]
exprsParser = nextParser
    where
        commaExprsParser :: Parser [Expr]
        commaExprsParser = do
                                _ <- schar ','
                                nextParser
                        <|> return []
        nextParser :: Parser [Expr]
        nextParser = do
                        e <- expr1Parser
                        c <- commaExprsParser
                        return $ e : c
                    <|> return []


afterIdentParser :: Ident -> Parser Expr
afterIdentParser ident =
    do _ <- schar '='; expr <- expr1Parser; return $ Assign ident expr
    <|> return (Var ident)
    <|> funCallParser ident
    where
        funCallParser :: Ident -> Parser Expr
        funCallParser i1 = do { _ <- schar '.'; i2 <- identParser; funCallParser (i1 ++ "." ++ i2) }
                          <|> do { _ <- schar '('; exprs <- exprsParser; _ <- schar ')'; return $ Call i1 exprs}


arrayComprParser :: Parser (Maybe ArrayCompr)
arrayComprParser = option (
    do
        _ <- symbol "if"
        _ <- schar '('
        e <- exprParser
        _ <- schar ')'
        a <- arrayComprParser
        return $ ArrayIf e a
    <|> do
        _ <- symbol "for"
        _ <- schar '('
        i <- identParser
        _ <- symbol "of"
        e <- exprParser
        _ <- schar ')'
        a <- arrayComprParser
        return $ ArrayForCompr (i, e, a))


expr1Parser :: Parser Expr
expr1Parser = expr2 `chainl1` mulOp `chainl1` addOp `chainl1` lessOp `chainl1` eqOp 
        where
            mulOp :: Parser (Expr -> Expr -> Expr)
            mulOp  = do { _ <- schar '*';    return $ expectTwo $ Call "*" }
                 <|> do { _ <- schar '%';    return $ expectTwo $ Call "%" }
            addOp  = do { _ <- schar '+';    return $ expectTwo $ Call "+" }
                 <|> do { _ <- schar '-';    return $ expectTwo $ Call "-" }
            lessOp = do { _ <- schar '<';    return $ expectTwo $ Call "<" }
            eqOp   = do { _ <- symbol "==="; return $ expectTwo $ Call "===" }
            expectTwo :: ([a] -> a) -> a -> a -> a
            expectTwo f a b = f (a:[b])

            expr2 = numberParser
                <|> do { _ <- schar '\''; s <- munch (/= '\''); _ <- schar '\''; return $ String s}
                <|> do { _ <- symbol "true"; return TrueConst }
                <|> do { _ <- symbol "false"; return FalseConst }
                <|> do { _ <- symbol "undefined"; return Undefined }
                <|> do { i <- identParser; afterIdentParser i }
                <|> do { _ <- schar '['; exprs <- exprsParser; _ <- schar ']'; return $ Array exprs}
                <|> do { _ <- schar '('; e <- exprParser; _ <- schar ')'; return e}
                <|> do
                    _ <- schar '['
                    _ <- symbol "for"
                    _ <- schar '('
                    i <- identParser
                    _ <- symbol "of"
                    e1 <- exprParser
                    _ <- schar ')'
                    a <- arrayComprParser
                    e2 <- exprParser
                    _ <- schar ']'
                    return $ Compr (i, e1, a) e2



exprParser :: Parser Expr
exprParser = do {e1 <- expr1Parser; _ <- schar ','; e2 <- exprParser; return $ Comma e1 e2}
       <|> expr1Parser


assignOptParser :: Parser (Maybe Expr)
assignOptParser = option $ schar '=' >> exprParser


stmtParser :: Parser Stm
stmtParser = do
                _ <- symbol "var"
                i <- identParser
                a <- assignOptParser
                return $ VarDecl i a
         <|> do { e <- exprParser; return $ ExprAsStm e}



stmtsParser :: Parser [Stm]
stmtsParser = do
                  stmt  <- stmtParser
                  _     <- schar ';'
                  stmts <- stmtsParser
                  return $ stmt:stmts
         <|> return []



programParser :: Parser Program
programParser = do {statements <- stmtsParser; return $ Prog statements}



parseString :: String -> Either ParseError Program
parseString input = let ret = do { cs <- programParser; _ <- spaces; return cs } in
    case parseEof ret input of
    [(prog, "")] -> return prog
    f -> Left $ ParseError $ show f  -- TODO: Better error message

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path
