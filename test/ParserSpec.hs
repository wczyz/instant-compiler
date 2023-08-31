{-# LANGUAGE OverloadedStrings #-}

module ParserSpec where

import qualified Data.Text as T
import Data.Void
import Parser
import Test.Hspec
import Test.Hspec.Megaparsec (shouldFailOn, shouldParse)
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
        shouldParse
          (run " 14 + 5 * 2")
          ( Program
              [ SExp
                  ( EBinOp
                      Add
                      (ELit 14)
                      (EBinOp Mul (ELit 5) (ELit 2))
                  )
              ]
          )
      it "division" $
        shouldParse
          (run "1/0")
          ( Program
              [ SExp
                  ( EBinOp
                      Div
                      (ELit 1)
                      (ELit 0)
                  )
              ]
          )
      it "expression statement with parentheses" $
        shouldParse
          (run "(b * b - 4 * a * c) / 2")
          ( Program
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
        shouldParse
          (run "  \n\n\n5;           33   -     4  * 2         ")
          ( Program
              [ SExp (ELit 5),
                SExp
                  ( EBinOp
                      Sub
                      (ELit 33)
                      (EBinOp Mul (ELit 4) (ELit 2))
                  )
              ]
          )
      it "addition binds to the right" $
        run "a + b + c"
          `shouldParse` Program [SExp (EBinOp Add (EVar "a") (EBinOp Add (EVar "b") (EVar "c")))]
    describe "failures" $ do
      it "unrecognized characters" $
        run `shouldFailOn` "&&"
      it "negative numbers in expressions 2" $
        run `shouldFailOn` "a=(-5)*2"
      it "semicolon at the end" $
        run `shouldFailOn` "5;"
      it "garbage at the end" $
        run `shouldFailOn` "5 * 13;^^^"
      it "unary minus at the end" $ do
        run `shouldFailOn` "5-"
        run `shouldFailOn` "5;4-"
      it "unary negative operator" $ do
        run `shouldFailOn` "-5"
        run `shouldFailOn` "- 5"
        run `shouldFailOn` "-5-4"
        run `shouldFailOn` "(-5)*2"

assignments :: Spec
assignments =
  describe "assignments" $ do
    describe "successes" $ do
      it "simple assignment statement" $
        shouldBe
          (run "a = 14 + 5 * 2")
          ( Right $
              Program
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
          ( Right $
              Program
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
          ( Right $
              Program
                [ SAss
                    "a"
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
          ( Right $
              Program
                [ SAss "x" (ELit 5),
                  SExp
                    ( EBinOp
                        Sub
                        (ELit 33)
                        (EBinOp Mul (ELit 4) (ELit 2))
                    )
                ]
          )
      it "assignment with addition binds to the right" $ do
        shouldBe
          (run "a = b + c + d")
          ( Right $
              Program
                [ SAss
                    "a"
                    ( EBinOp
                        Add
                        (EVar "b")
                        (EBinOp Add (EVar "c") (EVar "d"))
                    )
                ]
          )
    describe "failures" $ do
      it "semicolon at the end" $ do
        run `shouldFailOn` "a=5;"
        run `shouldFailOn` "a=5;;;"
      it "assignment to random characters" $
        run `shouldFailOn` "a=^^^" 
      it "garbage at the end" $
        run `shouldFailOn` "a=5;^^^"
      it "minus at the end" $ do
        run `shouldFailOn` "a=5-"
        run `shouldFailOn` "a=5;b=4-"
      it "unary negative operator" $ do
        run `shouldFailOn` "a=-5"
        run `shouldFailOn` "a=- 5"
        run `shouldFailOn` "a=-5-4"
        run `shouldFailOn` "a=(-5)*2"
