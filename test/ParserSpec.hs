{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import Data.Either (isLeft)
import qualified Data.Text as T
import Data.Void
import Parser
import Test.Hspec
import Text.Megaparsec
import Types

run :: T.Text -> Either (ParseErrorBundle T.Text Void) Program
run = parse pProg ""

spec :: Spec
spec = do
  describe "Instant parser tests" $ do
    expressions
    assignments

expressions :: Spec
expressions =
  describe "expressions" $ do
    describe "successes" $ do
      it "simple expression statement" $
        shouldBe
          (run " 14 + 5 * 2")
          ( Right $ Program
              [ SExp
                  ( EBinOp
                      Add
                      (ELit 14)
                      (EBinOp Mul (ELit 5) (ELit 2))
                  )
              ]
          )
      it "division" $
        shouldBe
          (run "1/0")
          ( Right $ Program
              [ SExp
                  ( EBinOp
                      Div
                      (ELit 1)
                      (ELit 0)
                  )
              ]
          )
      it "expression statement with parens" $
        shouldBe
          (run "(b * b - 4 * a * c) / 2")
          ( Right $ Program
              [ SExp
                  ( EBinOp
                      Div
                      ( EBinOp
                          Sub
                          (EBinOp Mul (EVar "b") (EVar "b"))
                          ( EBinOp
                              Mul
                              (EBinOp Mul (ELit 4) (EVar "a"))
                              (EVar "c")
                          )
                      )
                      (ELit 2)
                  )
              ]
          )
      it "expression statement with whitespaces" $
        shouldBe
          (run "  \n\n\n5;           33   -     4  * 2         ")
          ( Right $ Program
              [ SExp (ELit 5),
                SExp
                  ( EBinOp
                      Sub
                      (ELit 33)
                      (EBinOp Mul (ELit 4) (ELit 2))
                  )
              ]
          )
    describe "failures" $ do
      it "semicolon at the end" $
        shouldSatisfy
          (run "5;")
          isLeft

assignments :: Spec
assignments =
  describe "assignments" $ do
    describe "successes" $ do
      it "simple assignment statement" $
        shouldBe
          (run "a = 14 + 5 * 2")
          ( Right $ Program
              [ SAss
                  "a"
                  ( EBinOp
                      Add
                      (ELit 14)
                      (EBinOp Mul (ELit 5) (ELit 2))
                  )
              ]
          )
      it "division" $
        shouldBe
          (run "impossible = 1/0")
          ( Right $ Program
              [ SAss
                  "impossible"
                  ( EBinOp
                      Div
                      (ELit 1)
                      (ELit 0)
                  )
              ]
          )
      it "assignment statement with parens" $
        shouldBe
          (run "a = ((b * b - 4 * a * c) / 2)")
          ( Right $ Program
              [ SAss "a"
                  ( EBinOp
                      Div
                      ( EBinOp
                          Sub
                          (EBinOp Mul (EVar "b") (EVar "b"))
                          ( EBinOp
                              Mul
                              (EBinOp Mul (ELit 4) (EVar "a"))
                              (EVar "c")
                          )
                      )
                      (ELit 2)
                  )
              ]
          )
      it "assignment statement with whitespaces" $
        shouldBe
          (run "\n x \n =  \n\n\n5;           33   -     4  * 2         ")
          ( Right $ Program
              [ SAss "x" (ELit 5),
                SExp
                  ( EBinOp
                      Sub
                      (ELit 33)
                      (EBinOp Mul (ELit 4) (ELit 2))
                  )
              ]
          )
    describe "failures" $ do
      it "semicolon at the end" $
        shouldSatisfy
          (run "a=5;")
          isLeft
    describe "failures" $ do
      return ()
