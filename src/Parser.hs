{-# LANGUAGE OverloadedStrings #-}

module Parser
  ( pProg,
  )
where

import Control.Applicative hiding (many, some)
import Control.Monad (void)
import Control.Monad.Combinators.Expr
import qualified Data.Text as T
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Types

type Parser = Parsec Void T.Text

pProg :: Parser Program
pProg =
  Program
    <$> (pStmt `sepBy` some (symbol ";"))
    <* eof

pStmt :: Parser Stmt
pStmt =
  space
    *> (try pAss <|> SExp <$> pExpr)
  where
    pAss :: Parser Stmt
    pAss = do
      name <- pIdent
      void $ symbol "="
      SAss name <$> pExpr

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

pTerm :: Parser Expr
pTerm =
  choice
    [ parens pExpr,
      pLit,
      pVariable
    ]

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [binary "*" (EBinOp Mul) AssocLeft, binary "/" (EBinOp Div) AssocLeft],
    [binary "+" (EBinOp Add) AssocRight, binary "-" (EBinOp Sub) AssocRight]
  ]

binary :: T.Text -> (Expr -> Expr -> Expr) -> Assoc -> Operator Parser Expr
binary name f assoc = constructor (f <$ symbol name)
  where
    constructor =
      case assoc of
        AssocLeft -> InfixL
        AssocRight -> InfixR

pIdent :: Parser String
pIdent = lexeme $ do
  firstChar <- letterChar
  rest <- many alphaNumChar
  return $ firstChar : rest

pVariable :: Parser Expr
pVariable = EVar <$> pIdent

pLit :: Parser Expr
pLit = ELit <$> pInteger
  where
    pInteger :: Parser Integer
    pInteger = lexeme L.decimal

-- Helpers

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

symbol :: Tokens T.Text -> Parser (Tokens T.Text)
symbol = L.symbol space

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")
