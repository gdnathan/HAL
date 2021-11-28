--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Evaluate
--

module Interpreter.EvaluateExpr     ( evaluateExpr
                                    , EvaluationResult(..)
                                    ) where

import Interpreter.Register         ( RegisterId( RegisterId )
                                    , Register
                                    , EvaluatedValue( NoValue
                                                    , ValueName
                                                    )
                                    )
import Interpreter.EvaluateValue    ( evaluateValue
                                    , EvaluatingContext( Context )
                                    )
import Interpreter.Builtins.Define  ( define )
import Interpreter.Parser           ( buildExpressionsTrees
                                    , ProcedureArg( Symbol )
                                    , Tree(..)
                                    )
import Interpreter.Lexer            ( tokenize )

newtype EvaluationResult = Result (Register, EvaluatedValue)

evaluateExpr :: Register -> String -> EvaluationResult
evaluateExpr reg = loopEvaluation (Result (reg, NoValue)) . buildExpressionsTrees . tokenize

loopEvaluation :: EvaluationResult -> [Tree] -> EvaluationResult
loopEvaluation = foldl loopEvaluation'

loopEvaluation' :: EvaluationResult -> Tree -> EvaluationResult
loopEvaluation' (Result (reg, _)) tree = evaluateResult $ Context (reg, tree)

evaluateResult :: EvaluatingContext -> EvaluationResult
evaluateResult         (Context (reg, Node (Leaf (Symbol "define") : args)))  = defineResToEvaluationRes $ define reg args
evaluateResult context@(Context (reg, _))                                     = Result (reg, evaluateValue context)

defineResToEvaluationRes :: (Register, RegisterId) -> EvaluationResult
defineResToEvaluationRes (reg, RegisterId definedName) = Result (reg, ValueName definedName)
