--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Ord
--

module Interpreter.Builtins.Ord   ( ltProcedure
                                  , lteProcedure
                                  , gtProcedure
                                  , gteProcedure
                                  ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments
                                          , ArgumentIsNotNumber
                                          )
                                  )
import Interpreter.Register       ( Register
                                  , EvaluatedValue( ValueNumber
                                                  , ValueTrue
                                                  , ValueFalse
                                                  )
                                  )
import Interpreter.Parser         ( Tree )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )
import Interpreter.Lexer          ( NumbersType )

--- Builtin ---
ltProcedure :: Register -> [Tree] -> EvaluatedValue
ltProcedure = compareProcedure (<)

--- Builtin ---
lteProcedure :: Register -> [Tree] -> EvaluatedValue
lteProcedure = compareProcedure (<=)

--- Builtin ---
gtProcedure :: Register -> [Tree] -> EvaluatedValue
gtProcedure = compareProcedure (>)

--- Builtin ---
gteProcedure :: Register -> [Tree] -> EvaluatedValue
gteProcedure = compareProcedure (>=)

compareProcedure :: (NumbersType -> NumbersType -> Bool) -> Register -> [Tree] -> EvaluatedValue
compareProcedure func reg [left, right] = compareProcedure' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
compareProcedure _    _   _             = throw InvalidNumberOfArguments

compareProcedure' :: (NumbersType -> NumbersType -> Bool) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
compareProcedure' func (ValueNumber left) (ValueNumber right)
                                          | left `func` right = ValueTrue
                                          | otherwise         = ValueFalse
compareProcedure' _    _                  _                   = throw ArgumentIsNotNumber
