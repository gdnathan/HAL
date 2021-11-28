--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Arithmetic
--

module Interpreter.Builtins.Arithmetic  ( addition
                                        , subtraction
                                        , multiplication
                                        , division
                                        , modulo
                                        ) where

import Control.Exception          ( throw )
import Data.Fixed                 ( mod' )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments
                                          , ArgumentIsNotNumber
                                          , DividingByZero
                                          )
                                  )
import Interpreter.Register       ( Register
                                  , EvaluatedValue( ValueNumber )
                                  )
import Interpreter.Parser         ( Tree( Leaf )
                                  , ProcedureArg( Number )
                                  )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )
import Interpreter.Lexer          ( NumbersType )

--- Builtin ---
addition :: Register -> [Tree] -> EvaluatedValue
addition = arithmeticWithInfArgs (+)

--- Builtin ---
subtraction :: Register -> [Tree] -> EvaluatedValue
subtraction reg [tree]  = arithmeticWithInfArgs (-) reg [Leaf (Number 0), tree]
subtraction reg trees   = arithmeticWithInfArgs (-) reg trees

--- Builtin ---
multiplication :: Register -> [Tree] -> EvaluatedValue
multiplication = arithmeticWithInfArgs (*)

arithmeticWithInfArgs :: (NumbersType -> NumbersType -> NumbersType) -> Register -> [Tree] -> EvaluatedValue
arithmeticWithInfArgs _    _   []            = ValueNumber 0
arithmeticWithInfArgs _    reg [value]       = checkIsNum $ evaluateValue (Context (reg, value))
arithmeticWithInfArgs func reg (value : xs)  = arithmeticWithInfArgs' func (evaluateValue (Context (reg, value))) $ arithmeticWithInfArgs func reg xs

checkIsNum :: EvaluatedValue -> EvaluatedValue
checkIsNum value@(ValueNumber _)  = value
checkIsNum _                      = throw ArgumentIsNotNumber

arithmeticWithInfArgs' :: (NumbersType -> NumbersType -> NumbersType) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmeticWithInfArgs' func (ValueNumber a) (ValueNumber b)  = ValueNumber $ a `func` b
arithmeticWithInfArgs' _    _               _                = throw ArgumentIsNotNumber

--- Builtin ---
division :: Register -> [Tree] -> EvaluatedValue
division = arithmeticWith2Args (/)

--- Builtin ---
modulo :: Register -> [Tree] -> EvaluatedValue
modulo = arithmeticWith2Args mod'

arithmeticWith2Args :: (NumbersType -> NumbersType -> NumbersType) -> Register -> [Tree] -> EvaluatedValue
arithmeticWith2Args func reg [left, right] = arithmeticWith2Args' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
arithmeticWith2Args _    _   _             = throw InvalidNumberOfArguments

arithmeticWith2Args' :: (NumbersType -> NumbersType -> NumbersType) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmeticWith2Args' _    (ValueNumber _)    (ValueNumber 0)     = throw DividingByZero
arithmeticWith2Args' func (ValueNumber left) (ValueNumber right) = ValueNumber $ left `func` right
arithmeticWith2Args' _    _                  _                   = throw ArgumentIsNotNumber
