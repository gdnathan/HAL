--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Arithmetic
--

module Interpreter.Builtins.Arithmetic  ( add
                                        , sub
                                        , multiplication
                                        , divProcedure
                                        , modProcedure
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
add :: Register -> [Tree] -> EvaluatedValue
add = arithmetic (+)

--- Builtin ---
sub :: Register -> [Tree] -> EvaluatedValue
sub reg [tree]  = arithmetic (-) reg [Leaf (Number 0), tree]
sub reg trees   = arithmetic (-) reg trees

--- Builtin ---
multiplication :: Register -> [Tree] -> EvaluatedValue
multiplication = arithmetic (*)

arithmetic :: (NumbersType -> NumbersType -> NumbersType) -> Register -> [Tree] -> EvaluatedValue
arithmetic _    _   []            = ValueNumber 0
arithmetic _    reg [value]       = checkIsNum $ evaluateValue (Context (reg, value))
arithmetic func reg (value : xs)  = arithmetic' func (evaluateValue (Context (reg, value))) $ arithmetic func reg xs

checkIsNum :: EvaluatedValue -> EvaluatedValue
checkIsNum value@(ValueNumber _)  = value
checkIsNum _                      = throw ArgumentIsNotNumber

arithmetic' :: (NumbersType -> NumbersType -> NumbersType) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic' func (ValueNumber a) (ValueNumber b)  = ValueNumber $ a `func` b
arithmetic' _    _               _                = throw ArgumentIsNotNumber

--- Builtin ---
divProcedure :: Register -> [Tree] -> EvaluatedValue
divProcedure = arithmetic2 (/)

--- Builtin ---
modProcedure :: Register -> [Tree] -> EvaluatedValue
modProcedure = arithmetic2 mod'

arithmetic2 :: (NumbersType -> NumbersType -> NumbersType) -> Register -> [Tree] -> EvaluatedValue
arithmetic2 func reg [left, right] = arithmetic2' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
arithmetic2 _    _   _             = throw InvalidNumberOfArguments

arithmetic2' :: (NumbersType -> NumbersType -> NumbersType) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic2' func (ValueNumber left) (ValueNumber 0)     = throw DividingByZero
arithmetic2' func (ValueNumber left) (ValueNumber right) = ValueNumber $ left `func` right
arithmetic2' _    _                  _                   = throw ArgumentIsNotNumber
