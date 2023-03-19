module Types where

newtype Program
  = Program [Stmt]
  deriving (Eq, Show)

data Stmt
  = SAss String Expr
  | SExp Expr
  deriving (Eq, Show)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  deriving (Eq, Show)

data Expr
  = EBinOp BinOp Expr Expr
  | ELit Integer
  | EVar String
  deriving (Eq, Show)

data Assoc
  = AssocLeft
  | AssocRight
