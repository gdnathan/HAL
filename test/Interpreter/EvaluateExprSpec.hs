--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter EvaluateExprSpec
--

module Interpreter.EvaluateExprSpec  ( spec ) where

import Test.Hspec                     ( Spec
                                      , it
                                      , shouldBe
                                      )

import Interpreter.EvaluateExpr       ( evaluateExpr
                                      , EvaluationResult( Result )
                                      )
import Interpreter.Builtins.All       ( initialRegister )
import Interpreter.Register           ( EvaluatedValue )

getValue :: EvaluationResult -> EvaluatedValue
getValue (Result (_, value)) = value

spec :: Spec
spec = do
  it "Cons - two values" $ do
    show (getValue (evaluateExpr initialRegister "(cons 1 2)"))
      `shouldBe` "(1 . 2)"
  it "Cons - three value and nil" $ do
    show (getValue (evaluateExpr initialRegister "(cons 1 (cons 2 (cons 3 '())))"))
      `shouldBe` "(1 2 3)"
  it "Cons - complicated display" $ do
    show (getValue (evaluateExpr initialRegister "(cons (cons (cons 1 '()) 2) 3)"))
      `shouldBe` "(((1) . 2) . 3)"
  it "Car - list of two values" $ do
    show (getValue (evaluateExpr initialRegister "(car (cons 1 2))"))
      `shouldBe` "1"
  it "Cdr - list of two values" $ do
    show (getValue (evaluateExpr initialRegister "(cdr (cons 1 2))"))
      `shouldBe` "2"
  it "Cdr - quoted list" $ do
    show (getValue (evaluateExpr initialRegister "(cdr '(1 2 3))"))
      `shouldBe` "(2 3)"
  it "Eq? - two equals values" $ do
    show (getValue (evaluateExpr initialRegister "(eq? 1 1)"))
      `shouldBe` "#t"
  it "Eq? - operation" $ do
    show (getValue (evaluateExpr initialRegister "(eq? (+ 1 1) 2)"))
      `shouldBe` "#t"
  it "Eq? - quote equality" $ do
    show (getValue (evaluateExpr initialRegister "(eq? 'foo (car '(foo bar)))"))
      `shouldBe` "#t"
  it "Eq? - quote inequality" $ do
    show (getValue (evaluateExpr initialRegister "(eq? 'foo 'bar)"))
      `shouldBe` "#f"
  it "Eq? - two nils" $ do
    show (getValue (evaluateExpr initialRegister "(eq? '() '())"))
      `shouldBe` "#t"
  it "isAtom - quoted value" $ do
    show (getValue (evaluateExpr initialRegister "(atom? 'foo)"))
      `shouldBe` "#t"
  it "isAtom - list" $ do
    show (getValue (evaluateExpr initialRegister "(atom? '(1 2 3))"))
      `shouldBe` "#f"
  it "isAtom - empty list" $ do
    show (getValue (evaluateExpr initialRegister "(atom? '())"))
      `shouldBe` "#t"
  it "Sub - sub one element" $ do
    show (getValue (evaluateExpr initialRegister "(- (cond (3)))"))
      `shouldBe` "-3"
  it "Add - empty arithmetic" $ do
    show (getValue (evaluateExpr initialRegister "(+ )"))
      `shouldBe` "0"
  it "Multiplication - many arguments" $ do
    show (getValue (evaluateExpr initialRegister "(* 2 5 (cond (2)) 3)"))
      `shouldBe` "60"
  it "Div - on multiple operation" $ do
    show (getValue (evaluateExpr initialRegister "(div (* 5 2) 3)"))
      `shouldBe` "3"
  it "Mod - 10 % 3" $ do
    show (getValue (evaluateExpr initialRegister "(mod (+ 5 5) (+ 3))"))
      `shouldBe` "1"
  it "< - true comparison" $ do
    show (getValue (evaluateExpr initialRegister "(< (* 2 2) 5)"))
      `shouldBe` "#t"
  it "< - false comparison" $ do
    show (getValue (evaluateExpr initialRegister "(< 5 5)"))
      `shouldBe` "#f"
  it "<= - equal values" $ do
    show (getValue (evaluateExpr initialRegister "(<= 5 5)"))
      `shouldBe` "#t"
  it "> - false comparison" $ do
    show (getValue (evaluateExpr initialRegister "(> (* 2 2) 5)"))
      `shouldBe` "#f"
  it "> - equal values" $ do
    show (getValue (evaluateExpr initialRegister "(> 5 5)"))
      `shouldBe` "#f"
  it ">= - equal values" $ do
    show (getValue (evaluateExpr initialRegister "(>= 5 5)"))
      `shouldBe` "#t"
  it "Quote - word" $ do
    show (getValue (evaluateExpr initialRegister "(quote toto)"))
      `shouldBe` "toto"
  it "Quote - builtin procedure" $ do
    show (getValue (evaluateExpr initialRegister "(quote (+ 1 2))"))
      `shouldBe` "(+ 1 2)"
  it "Quote - word" $ do
    show (getValue (evaluateExpr initialRegister "'toto"))
      `shouldBe` "toto"
  it "Quote - builtin procedure" $ do
    show (getValue (evaluateExpr initialRegister "'(+ 1 2)"))
      `shouldBe` "(+ 1 2)"
  it "Quote - quote of list" $ do
    show (getValue (evaluateExpr initialRegister "(quote (a b c))"))
      `shouldBe` "(a b c)"
  it "Quote - quote of empty list" $ do
    show (getValue (evaluateExpr initialRegister "(quote ())"))
      `shouldBe` "()"
  it "Quote - quote of quote" $ do
    show (getValue (evaluateExpr initialRegister "(quote (quote a))"))
      `shouldBe` "'a"
  it "Quote - car of quote of quote" $ do
    show (getValue (evaluateExpr initialRegister "(car (quote (quote a)))"))
      `shouldBe` "quote"
  it "Quote - cdr of quote of quote" $ do
    show (getValue (evaluateExpr initialRegister "(cdr (quote (quote a)))"))
      `shouldBe` "(a)"
  it "Quote - quote of quote of quote" $ do
    show (getValue (evaluateExpr initialRegister "(quote (quote '(1 2)))"))
      `shouldBe` "''(1 2)"
  it "Quote - complex expression" $ do
    show (getValue (evaluateExpr initialRegister "'(a b '(a b) (cons 1))"))
      `shouldBe` "(a b '(a b) (cons 1))"
  it "Quote - operation on quoted list" $ do
    show (getValue (evaluateExpr initialRegister "(+ (car (car (cdr (quote '(1 b))))) 1)"))
      `shouldBe` "2"
  it "Lambda - basic procedure" $ do
    show (getValue (evaluateExpr initialRegister "(lambda (a b) (+ a b))"))
      `shouldBe` "#<procedure>"
  it "Lambda - executed procedure" $ do
    show (getValue (evaluateExpr initialRegister "((lambda (a b) (+ a b)) 1 2)"))
      `shouldBe` "3"
  it "Lambda - executed procedure with arguments as list" $ do
    show (getValue (evaluateExpr initialRegister "((lambda list (cdr list)) 1 2 3)"))
      `shouldBe` "(2 3)"
  it "Define - simple define" $ do
    show (getValue (evaluateExpr initialRegister "(define foo 42)"))
      `shouldBe` "foo"
  it "Define - simple define with use" $ do
    show (getValue (evaluateExpr initialRegister "(define foo 42) (* foo 2)"))
      `shouldBe` "84"
  it "Define - procedure define (using lambda)" $ do
    show (getValue (evaluateExpr initialRegister "(define add (lambda (a b) (+ a b)))"))
      `shouldBe` "add"
  it "Define - procedure define (using lambda) with use" $ do
    show (getValue (evaluateExpr initialRegister "(define add (lambda (a b) (+ a b))) (add 1 3)"))
      `shouldBe` "4"
  it "Define - procedure define" $ do
    show (getValue (evaluateExpr initialRegister "(define (sub a b) (- a b))"))
      `shouldBe` "sub"
  it "Define - procedure define with use" $ do
    show (getValue (evaluateExpr initialRegister "(define (sub a b) (- a b)) (sub 3 1)"))
      `shouldBe` "2"
  it "Let - expression" $ do
    show (getValue (evaluateExpr initialRegister "(let ((a 2) (b (+ 1 2))) (+ a b))"))
      `shouldBe` "5"
  it "Cond - simple condition" $ do
    show (getValue (evaluateExpr initialRegister "(cond (#f 1) (#t (+ 1 1)))"))
      `shouldBe` "2"
  it "Cond - return first value" $ do
    show (getValue (evaluateExpr initialRegister "(cond ('foo)"))
      `shouldBe` "foo"
  it "Cond - return no value" $ do
    show (getValue (evaluateExpr initialRegister "(cond (#f)"))
      `shouldBe` ""
  it "Cond - complex condition" $ do
    show (getValue (evaluateExpr initialRegister "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))"))
      `shouldBe` "here"
  it "Cond - complex condition" $ do
    show (getValue (evaluateExpr initialRegister "(cond ((eq? 'foo (car '(foo bar))) 'here) ((eq? 1 2) 'there) (#t 'nope))"))
      `shouldBe` "here"

  it "Correct - cons with values" $ do
    show (getValue (evaluateExpr initialRegister
      "(define (null? l) (eq? l '()))                                               \
      \                                                                             \
      \(define (merge-lists l1 l2)                                                  \
      \  (cond ((null? l1) l2)                                                      \
      \        ((null? l2) l1)                                                      \
      \        ((< (car l1) (car l2)) (cons (car l1) (merge-lists (cdr l1) l2)))    \
      \        (#t                    (cons (car l2) (merge-lists l1 (cdr l2))))))  \
      \                                                                             \
      \(define (split-half l l1 l2)                                                 \
      \  (cond ((null? l) (cons l1 l2))                                             \
      \        ((null? (cdr l)) (split-half (cdr l) (cons (car l) l1) l2))          \
      \        (#t (split-half (cdr (cdr l))                                        \
      \                        (cons (car l) l1)                                    \
      \                        (cons (car (cdr l)) l2)))))                          \
      \                                                                             \
      \(define (merge-sort lst)                                                     \
      \  (cond ((null? lst) '())                                                    \
      \        ((null? (cdr lst)) lst)                                              \
      \        (#t (let ((lsts (split-half lst '() '())))                           \
      \              (merge-lists (merge-sort (car lsts))                           \
      \                           (merge-sort (cdr lsts)))))))                      \
      \                                                                             \
      \(merge-sort '(39 16 22 24 17 29 18 26 27 3 34 25 10 6 7 12 8 30 2 21 13 36 14 38 32 41 40 4 35 19 5 33 23 9 15 31 28 20 42 37 11 1))"))
      `shouldBe` "(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42)"
