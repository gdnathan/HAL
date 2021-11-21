--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Evaluate
--

module Interpreter.EvaluateExpr     ( evaluateExpr
                                    , EvaluationResult(..)
                                    ) where

import Interpreter.Data.Tree        ( ProcedureArg (..)
                                    , Tree (..)
                                    )
import Interpreter.Data.Register    ( Register(..)
                                    , EvaluatedValue(..)
                                    , regLookup, regInsert
                                    )
import Interpreter.EvaluateValue    ( EvaluatingContext(..), evaluateValue )
import Interpreter.Builtins.Define  ( define )

newtype EvaluationResult  = Result  (Register, EvaluatedValue)
  deriving (Show)

evaluateExpr :: EvaluatingContext -> EvaluationResult
evaluateExpr         (Context (reg, Node (Leaf (Symbol "define") : args)))  = defineResToEvaluationRes $ define reg args
evaluateExpr context@(Context (reg, _))                                     = Result (reg, evaluateValue context)

defineResToEvaluationRes :: (Register, String) -> EvaluationResult
defineResToEvaluationRes (reg, definedName) = Result (reg, ValueName definedName)
