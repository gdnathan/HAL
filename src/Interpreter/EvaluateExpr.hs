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
import Interpreter.Parser           ( buildExpressionsTrees )
import Interpreter.Lexer            ( tokenize )

newtype EvaluationResult  = Result  (Register, EvaluatedValue)
  deriving (Show)

evaluateExpr :: Register -> String -> EvaluationResult
evaluateExpr reg = loopEvaluation (Result (reg, NoValue)) . buildExpressionsTrees . tokenize

loopEvaluation :: EvaluationResult -> [Tree] -> EvaluationResult
loopEvaluation = foldl loopEvaluation'

loopEvaluation' :: EvaluationResult -> Tree -> EvaluationResult
loopEvaluation' (Result (reg, _)) tree = evaluateContext $ Context (reg, tree)

evaluateContext :: EvaluatingContext -> EvaluationResult
evaluateContext         (Context (reg, Node (Leaf (Symbol "define") : args)))  = defineResToEvaluationRes $ define reg args
evaluateContext context@(Context (reg, _))                                     = Result (reg, evaluateValue context)

defineResToEvaluationRes :: (Register, String) -> EvaluationResult
defineResToEvaluationRes (reg, definedName) = Result (reg, ValueName definedName)
