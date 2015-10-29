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

Expr       ::= Expr `,` Expr
            |  Expr1

Expr1      ::= Number
            |  String
            |  `true`
            |  `false`
            |  `undefined`
            |  Expr1 `+` Expr1
            |  Expr1 `-` Expr1
            |  Expr1 `*` Expr1
            |  Expr1 `%` Expr1
            |  Expr1 `<` Expr1
            |  Expr1 `===` Expr1
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

--import Control.Applicative ( Applicative(..), Alternative((<|>), empty, many) )
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



parseString :: String -> Either ParseError Program
parseString = undefined

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = parseString <$> readFile path
