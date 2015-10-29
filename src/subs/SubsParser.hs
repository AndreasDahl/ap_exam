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

Stm  ::= var Ident AssignOpt
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
            |  Ident AfterIdent
            |  `[` Exprs `]`
            |  `[` `for` `(` Ident `of` Expr `)` ArrayCompr Expr `]`
            |  `(` Expr `)`

Expr2     ::=  Number
            |  String
            |  `true`
            |  `false`
            |  `undefined`


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
exprsParser = do
    op <- option $ expr1Parser >> commaExprsParser
    case op of
        Just p -> return p
        _      -> return []
    where
        commaExprsParser :: Parser [Expr]
        commaExprsParser = do
            op <- option $ schar ',' >> expr1Parser >> commaExprsParser
            case op of
                Just p -> return p
                _      -> return []


afterIdentParser :: Ident -> Parser Expr
afterIdentParser ident =
    do _ <- schar '='; expr <- expr1Parser; return $ Assign ident expr
    <|> return (Var ident)


expr1Parser :: Parser Expr
expr1Parser = expr2
        <|> do { e1 <- expr2; _ <- schar '*'; e2 <- expr1Parser; return $ Call "*" (e1:[e2])}
        <|> do { e1 <- expr2; _ <- schar '%'; e2 <- expr1Parser; return $ Call "%" (e1:[e2])}
        <|> do { e1 <- expr2; _ <- schar '+'; e2 <- expr1Parser; return $ Call "+" (e1:[e2])}
        <|> do { e1 <- expr2; _ <- schar '-'; e2 <- expr1Parser; return $ Call "-" (e1:[e2])}
        <|> do { e1 <- expr2; _ <- schar '<'; e2 <- expr1Parser; return $ Call "<" (e1:[e2])}
        <|> do { e1 <- expr2; _ <- symbol "==="; e2 <- expr1Parser; return $ Call "===" (e1:[e2])}
        where
            expr2 = numberParser
                <|> do { _ <- schar '"'; s <- munch (/= '"'); _ <- schar '"'; return $ String s}
                <|> do { _ <- symbol "true"; return TrueConst }
                <|> do { _ <- symbol "false"; return FalseConst }
                <|> do { _ <- symbol "undefined"; return Undefined }
                <|> do { i <- identParser; afterIdentParser i }
                <|> do { _ <- schar '['; exprs <- exprsParser; _ <- schar ']'; return $ Array exprs}


exprParser :: Parser Expr
exprParser = do {e1 <- expr1Parser; _ <- schar ','; e2 <- exprParser; return $ Comma e1 e2}
       <|> expr1Parser




parseString :: String -> Either ParseError Program
parseString = undefined

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path
