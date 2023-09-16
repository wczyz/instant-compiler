{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module LLVM (
    generateLLVMCode,
) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import Types

generateLLVMCode :: Program -> T.Text
generateLLVMCode = getInsideCode . _code . runLLVM . codegen

-- State

data CodegenState = CodegenState
    { _varCount :: Int
    -- ^ Counter for generating unique variable names
    , _nameMap :: Map.Map String String
    -- ^ Mapping from variable names to LLVM variable names
    }

initCodegenState :: CodegenState
initCodegenState = CodegenState{_varCount = 1, _nameMap = Map.empty}

-- Code

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

-- LLVM monad

newtype LLVM a = LLVM (State CodegenState a)
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

runLLVM :: LLVM a -> a
runLLVM (LLVM m) = evalState m initCodegenState

data LLVMResult = LLVMResult
    { _name :: Maybe T.Text
    -- ^ Optional name to which result value is bound
    , _code :: Code
    -- ^ LLVM IR code
    }

-- Codegen

class Codegen a where
    codegen :: a -> LLVM LLVMResult

instance (Codegen a) => Codegen [a] where
    codegen :: (Codegen a) => [a] -> LLVM LLVMResult
    codegen [] = returnCode mempty
    codegen [mx] = codegen mx
    codegen (mx : mxs) = do
        x <- _code <$> codegen mx
        xs <- _code <$> codegen mxs
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
        rest <- _code <$> codegen stmts
        returnCode $ mconcat [initVals, mainStart, rest, mainEnd]

instance Codegen Stmt where
    codegen :: Stmt -> LLVM LLVMResult
    codegen (SExp expr) = do
        eResult <- codegen expr
        let printCode = Code $ T.pack ("call i32 @printInt(i32 %" ++ resultName eResult ++ ")")
        returnCode $ _code eResult <> printCode
    codegen (SAss name expr) = do
        eResult <- codegen expr
        modify (\s -> s{_nameMap = Map.insert name (resultName eResult) (_nameMap s)})
        return LLVMResult{_name = Just (T.pack name), _code = _code eResult}

instance Codegen Expr where
    codegen :: Expr -> LLVM LLVMResult
    codegen (ELit value) = do
        count <- getCount
        let alloc = Code $ T.pack ("%" ++ show count ++ " = " ++ "alloca i32")
            store = Code $ T.pack ("store i32 " ++ show value ++ ", i32* %" ++ show count)
        newCount <- getCount
        let load = Code $ T.pack ("%" ++ show newCount ++ " = " ++ "load i32, i32* %" ++ show count)
        return LLVMResult{_name = (Just . T.pack . show) newCount, _code = mconcat [alloc, store, load]}
    codegen (EVar _name) = do
        nameMap <- gets _nameMap
        let name = fromJust (Map.lookup _name nameMap)
        return LLVMResult{_name = Just (T.pack name), _code = mempty}
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
                { _name = (Just . T.pack . show) count
                , _code = mconcat [_code e1Result, _code e2Result, currentCode]
                }

-- Helpers

returnCode :: Code -> LLVM LLVMResult
returnCode x = return LLVMResult{_name = Nothing, _code = x}

binOpToLLVMOp :: BinOp -> String
binOpToLLVMOp op =
    case op of
        Add -> "add"
        Sub -> "sub"
        Mul -> "mul"
        Div -> "udiv"

-- WARNING: partial function
resultName :: LLVMResult -> String
resultName = T.unpack . fromJust . _name

incVarCount :: LLVM ()
incVarCount = do
    s <- get
    let newCount = _varCount s + 1
    put s{_varCount = newCount}

getCount :: LLVM Int
getCount = gets _varCount <* incVarCount
