{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import LLVM
import Parser
import System.Environment (getArgs)
import Text.Megaparsec (errorBundlePretty, parse)
import Runner

main :: IO ()
main = do
    inputData <- prepareAST

    
    args <- getArgs

    -- read from file if specified
    -- otherwise read from stdin
    input <- case args of
        (filename : _) -> TIO.readFile filename
        _ -> TIO.getContents

    let program = parse pProg "" input

    case program of
        (Right v) -> TIO.writeFile "x.bc" $ generateLLVMCode v
        (Left e) -> putStrLn $ errorBundlePretty e
