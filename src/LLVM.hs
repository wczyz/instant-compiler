{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module LLVM (
  generateProgram,
)
where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Types

generateProgram :: Program -> T.Text
generateProgram = getInsideCode . code . runLLVM . codegen

-- State

data CodegenState = CodegenState
  { varCount :: Int
  , nameMap :: Map.Map String String
  }

initCodegenState :: CodegenState
initCodegenState = CodegenState{varCount = 1, nameMap = Map.empty}

-- LLVM monad

newtype LLVM a = LLVM (State CodegenState a)
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

runLLVM :: LLVM a -> a
runLLVM (LLVM m) = evalState m initCodegenState

-- Codegen

newtype Code = Code T.Text

getInsideCode :: Code -> T.Text
getInsideCode (Code x) = x

instance Semigroup Code where
  (<>) :: Code -> Code -> Code
  (<>) (Code x) (Code y) = Code $ T.intercalate "\n" [x, y]

instance Monoid Code where
  mempty :: Code
  mempty = Code ""

instance Show Code where
  show :: Code -> String
  show (Code x) = T.unpack x

data LLVMResult = LLVMResult
  { name :: Maybe T.Text -- Optional name to which result value is bound
  , code :: Code -- Generated code
  }

class Codegen a where
  codegen :: a -> LLVM LLVMResult

instance Codegen a => Codegen [a] where
  codegen :: Codegen a => [a] -> LLVM LLVMResult
  codegen [] = returnCode mempty
  codegen [mx] = codegen mx
  codegen (mx : mxs) = do
    x <- code <$> codegen mx
    xs <- code <$> codegen mxs
    returnCode $ x <> xs

instance Codegen Program where
  codegen :: Program -> LLVM LLVMResult
  codegen (Program stmts) = do
    let initVals =
          Code
            "@formatString = private constant [4 x i8] c\"%d\\0A\\00\" \n\
            \declare i32 @printf(i8*, ...)\n\
            \define i32 @printInt(i32 %x) {\n\
            \%call = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @formatString , i32 0, i32 0), i32 %x)\n\
            \ret i32 %call\n\
            \}"
        mainStart = Code "define i32 @main() {"
        mainEnd =
          Code
            "ret i32 0\n\
            \}"
    rest <- code <$> codegen stmts
    returnCode $ mconcat [initVals, mainStart, rest, mainEnd]

instance Codegen Stmt where
  codegen :: Stmt -> LLVM LLVMResult
  codegen (SExp expr) = do
    eResult <- codegen expr
    let printCode = Code $ T.pack ("call i32 @printInt(i32 %" ++ resultName eResult ++ ")")
    returnCode $ code eResult <> printCode
  codegen (SAss _name expr) = do
    eResult <- codegen expr
    modify (\s -> s{nameMap = Map.insert _name (resultName eResult) (nameMap s)})
    return LLVMResult{name = Just (T.pack _name), code = code eResult}

instance Codegen Expr where
  codegen :: Expr -> LLVM LLVMResult
  codegen (ELit value) = do
    count <- getCount
    let alloc = Code $ T.pack ("%" ++ show count ++ " = " ++ "alloca i32")
        store = Code $ T.pack ("store i32 " ++ show value ++ ", i32* %" ++ show count)
    newCount <- getCount
    let load = Code $ T.pack ("%" ++ show newCount ++ " = " ++ "load i32, i32* %" ++ show count)
    return LLVMResult{name = (Just . T.pack . show) newCount, code = mconcat [alloc, store, load]}
  codegen (EVar _name) = do
    nameMapping <- gets nameMap
    let boundName = fromJust (Map.lookup _name nameMapping)
    return LLVMResult{name = Just (T.pack boundName), code = mempty}
  codegen (EBinOp op e1 e2) = do
    e1Result <- codegen e1
    e2Result <- codegen e2
    count <- getCount
    let currentCode =
          (Code . T.pack) $
            "%"
              ++ show count
              ++ " = "
              ++ binOpToLLVMOp op
              ++ " i32 "
              ++ "%"
              ++ resultName e1Result
              ++ ", "
              ++ "%"
              ++ resultName e2Result
    return $
      LLVMResult
        { name = (Just . T.pack . show) count
        , code = mconcat [code e1Result, code e2Result, currentCode]
        }

-- Helpers

returnCode :: Code -> LLVM LLVMResult
returnCode x = return LLVMResult{name = Nothing, code = x}

binOpToLLVMOp :: BinOp -> String
binOpToLLVMOp op =
  case op of
    Add -> "add"
    Sub -> "sub"
    Mul -> "mul"
    Div -> "udiv"

-- TODO: Handle case when name is Nothing
-- Remove partial function `fromJust`
resultName :: LLVMResult -> String
resultName = T.unpack . fromJust . name

incVarCount :: LLVM ()
incVarCount = do
  s <- get
  let newCount = varCount s + 1
  put s{varCount = newCount}

getCount :: LLVM Int
getCount = gets varCount <* incVarCount
