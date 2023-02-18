{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as TIO
import Parser (pProg)
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  input <- TIO.getContents

  let program = parse pProg "" input

  case program of
    (Right v) -> mapM_ print v
    (Left e) -> putStrLn $ errorBundlePretty e
