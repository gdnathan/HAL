--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Lambda
--

module Interpreter.Builtins.Lambda  ( lambda
                                    , letProcedure
                                    ) where

import Control.Exception            ( throw )

import Interpreter.Error            ( Error ( InvalidNumberOfArguments, InvalidSyntax ) )
import Interpreter.Data.Register    ( Register
                                    , EvaluatedValue
                                    , regInsertRange2
                                    , RegisterId (RegisterId)
                                    )
import Interpreter.Data.Tree        ( Tree ( Node, Leaf )
                                    , ProcedureArg (Symbol)
                                    )
import Interpreter.Builtins.Define  ( createProcedure, createProcedure' )
import Interpreter.EvaluateValue    ( createList
                                    , evaluateValue
                                    , EvaluatingContext( Context )
                                    )

--- Builtin ---
lambda :: Register -> [Tree] -> EvaluatedValue
lambda reg [Node argsName             , body] = createProcedure argsName body
lambda _   [listName@(Leaf (Symbol _)), body] = createProcedure' (\reg args -> [createList reg args]) [listName] body
lambda _   [_                         , _]    = throw $ InvalidSyntax "variable name must be a string"
lambda _    _                                 = throw InvalidNumberOfArguments

--- Builtin ---
letProcedure :: Register -> [Tree] -> EvaluatedValue
letProcedure reg [Node args, body]  = evaluateValue (Context (regInsertRange2 reg $ getLetArgs reg args, body))
letProcedure _    _                 = throw InvalidNumberOfArguments

getLetArgs :: Register -> [Tree] -> ([RegisterId], [EvaluatedValue])
getLetArgs reg []                                         = ([], [])
getLetArgs reg (Node [Leaf (Symbol varName), value] : xs) = getLetArgs' reg xs (RegisterId varName) $ evaluateValue $ Context (reg, value)
getLetArgs _    _                                         = throw $ InvalidSyntax "missing bound value definition"

getLetArgs' :: Register -> [Tree] -> RegisterId -> EvaluatedValue -> ([RegisterId], [EvaluatedValue])
getLetArgs' reg xs varName value = let (ids, values) = getLetArgs reg xs in (varName : ids, value : values)
