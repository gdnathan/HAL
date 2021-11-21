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
                                          )
                                  )
import Interpreter.Data.Register  ( Register
                                  , EvaluatedValue( ValueNumber )
                                  )
import Interpreter.Data.Tree      ( Tree )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )

--- Builtin ---
add :: Register -> [Tree] -> EvaluatedValue
add = arithmetic (+)

--- Builtin ---
sub :: Register -> [Tree] -> EvaluatedValue
sub = arithmetic (-)

--- Builtin ---
multiplication :: Register -> [Tree] -> EvaluatedValue
multiplication = arithmetic (*)

arithmetic :: (Float -> Float -> Float) -> Register -> [Tree] -> EvaluatedValue
arithmetic _    _   []            = ValueNumber 0
arithmetic _    reg [value]       = checkIsNum $ evaluateValue (Context (reg, value))
arithmetic func reg (value : xs)  = arithmetic' func (evaluateValue (Context (reg, value))) $ arithmetic func reg xs

checkIsNum :: EvaluatedValue -> EvaluatedValue
checkIsNum value@(ValueNumber _)  = value
checkIsNum _                      = throw ArgumentIsNotNumber

arithmetic' :: (Float -> Float -> Float) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic' func (ValueNumber a) (ValueNumber b)  = ValueNumber $ a `func` b
arithmetic' _    _               _                = throw ArgumentIsNotNumber

--- Builtin ---
divProcedure :: Register -> [Tree] -> EvaluatedValue
divProcedure = arithmetic2 (/)

--- Builtin ---
modProcedure :: Register -> [Tree] -> EvaluatedValue
modProcedure = arithmetic2 mod'

arithmetic2 :: (Float -> Float -> Float) -> Register -> [Tree] -> EvaluatedValue
arithmetic2 func reg [left, right] = arithmetic2' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
arithmetic2 _    _   _             = throw InvalidNumberOfArguments

arithmetic2' :: (Float -> Float -> Float) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic2' func (ValueNumber left) (ValueNumber right) = ValueNumber $ left `func` right
arithmetic2' _    _                  _                   = throw ArgumentIsNotNumber
