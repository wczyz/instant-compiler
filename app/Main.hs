{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Parser (pProg)
import Text.Megaparsec (errorBundlePretty, parse)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs

  -- read from file if specified
  -- otherwise read from stdin
  input <- case args of
    (filename:_) -> TIO.readFile filename
    _ -> TIO.getContents

  let program = parse pProg "" input

  case program of
    (Right v) -> mapM_ print v
    (Left e) -> putStrLn $ errorBundlePretty e
