--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter LexerSpec
--

module Interpreter.LexerSpec    ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                )

import Interpreter.Lexer        ( tokenize
                                , Token(..)
                                )

newtype TestToken = TestToken Token
instance Eq TestToken where
   (TestToken ParenthesisOpen)  == (TestToken ParenthesisOpen)  = True
   (TestToken ParenthesisClose) == (TestToken ParenthesisClose) = True
   (TestToken Quote)            == (TestToken Quote)            = True
   (TestToken (Number n1))      == (TestToken (Number n2))      = n1 == n2
   (TestToken (Symbol str1))    == (TestToken (Symbol str2))    = str1 == str2
   _                            == _                            = False

spec :: Spec
spec = do
    it "Correct - cons with values" $ do
        map TestToken (tokenize "(cons 1 2)")
            == map TestToken [
              ParenthesisOpen,
                Symbol "cons", Number 1, Number 2,
              ParenthesisClose
            ]
    it "Correct - doubly nested cons" $ do
        map TestToken (tokenize "(cons (cons 1 2) (cons 3 4))")
            == map TestToken [
              ParenthesisOpen,
                Symbol "cons", 
                ParenthesisOpen,
                  Symbol "cons", Number 1, Number 2,
                ParenthesisClose,
                ParenthesisOpen,
                  Symbol "cons", Number 3, Number 4,
                ParenthesisClose,
              ParenthesisClose
            ]
    it "Correct - doubly nested cons with nil" $ do
        map TestToken (tokenize "(cons (cons 1 '()) (cons 3 4))")
            == map TestToken [
              ParenthesisOpen,
                Symbol "cons", 
                ParenthesisOpen,
                  Symbol "cons", Number 1, Quote, ParenthesisOpen, ParenthesisClose,
                ParenthesisClose,
                ParenthesisOpen,
                  Symbol "cons", Number 3, Number 4,
                ParenthesisClose,
              ParenthesisClose
            ]
    it "Correct - foo define" $ do
        map TestToken (tokenize "((define foo '21))")
            == map TestToken [
              ParenthesisOpen,
                ParenthesisOpen,
                  Symbol "define", Symbol "foo", Quote, Number 21,
                ParenthesisClose,
              ParenthesisClose
            ]
    it "Correct - foo cond" $ do
        map TestToken (tokenize "(cond (#f 'foo) (#f foo) (#t 42) (#t '42))")
            == map TestToken [
              ParenthesisOpen,
                Symbol "cond",
                  ParenthesisOpen,
                    Symbol "#f", Quote, Symbol "foo",
                  ParenthesisClose,
                  ParenthesisOpen,
                    Symbol "#f", Symbol "foo",
                  ParenthesisClose,
                  ParenthesisOpen,
                    Symbol "#t", Number 42,
                  ParenthesisClose,
                  ParenthesisOpen,
                    Symbol "#t", Quote, Number 42,
                  ParenthesisClose,
              ParenthesisClose
            ]
    it "Correct - factorial define" $ do
        map TestToken (tokenize "(define (fact x) (cond ((eq? x 1) 1) (#t (* x (fact (- x 1))))))")
            == map TestToken [
              ParenthesisOpen,
                Symbol "define",
                ParenthesisOpen,
                  Symbol "fact", Symbol "x",
                ParenthesisClose,
                ParenthesisOpen,
                  Symbol "cond",
                  ParenthesisOpen,
                    ParenthesisOpen,
                      Symbol "eq?", Symbol "x", Number 1,
                    ParenthesisClose,
                    Number 1,
                  ParenthesisClose,
                  ParenthesisOpen,
                    Symbol "#t",
                    ParenthesisOpen,
                      Symbol "*", Symbol "x",
                      ParenthesisOpen,
                        Symbol "fact",
                        ParenthesisOpen,
                          Symbol "-", Symbol "x", Number 1,
                        ParenthesisClose,
                      ParenthesisClose,
                    ParenthesisClose,
                  ParenthesisClose,
                ParenthesisClose,
              ParenthesisClose
            ]
