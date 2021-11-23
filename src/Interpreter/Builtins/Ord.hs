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
import Interpreter.Data.Register  ( Register
                                  , EvaluatedValue( ValueNumber
                                                  , ValueTrue
                                                  , ValueFalse
                                                  )
                                  )
import Interpreter.Data.Tree      ( Tree )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )

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

compareProcedure :: (Double -> Double -> Bool) -> Register -> [Tree] -> EvaluatedValue
compareProcedure func reg [left, right] = compareProcedure' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
compareProcedure _    _   _             = throw InvalidNumberOfArguments

compareProcedure' :: (Double -> Double -> Bool) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
compareProcedure' func (ValueNumber left) (ValueNumber right)
                                          | left `func` right = ValueTrue
                                          | otherwise         = ValueFalse
compareProcedure' _    _                  _                   = throw ArgumentIsNotNumber
