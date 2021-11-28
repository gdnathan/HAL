--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Lambda
--

module Interpreter.Builtins.Lambda  ( lambda
                                    , letProcedure
                                    ) where

import Control.Exception            ( throw )

import Interpreter.Error            ( Error ( InvalidNumberOfArguments, InvalidSyntax ) )
import Interpreter.Register         ( regInsertRange2
                                    , Register
                                    , EvaluatedValue
                                    , RegisterId( RegisterId )
                                    )
import Interpreter.Parser           ( Tree ( Node, Leaf )
                                    , ProcedureArg (Symbol)
                                    )
import Interpreter.Builtins.Define  ( createProcedure, createProcedure' )
import Interpreter.EvaluateValue    ( evaluateValue
                                    , EvaluatingContext( Context )
                                    )
import Interpreter.Builtins.Quote   ( createList )

--- Builtin ---
lambda :: Register -> [Tree] -> EvaluatedValue
lambda reg [Node argsName             , body] = createProcedure argsName body
lambda _   [listName@(Leaf (Symbol _)), body] = createProcedure' (\reg args -> [createList args]) [listName] body
lambda _   [_                         , _   ] = throw $ InvalidSyntax "variable name must be a string"
lambda _    _                                 = throw InvalidNumberOfArguments

--- Builtin ---
letProcedure :: Register -> [Tree] -> EvaluatedValue
letProcedure reg [Node args, body]  = evaluateValue (Context (regInsertRange2 reg $ getLetArgs reg args, body))
letProcedure _    _                 = throw InvalidNumberOfArguments

getLetArgs :: Register -> [Tree] -> ([RegisterId], [EvaluatedValue])
getLetArgs reg = foldl (getLetArgs' reg) ([], [])

getLetArgs' :: Register -> ([RegisterId], [EvaluatedValue]) -> Tree -> ([RegisterId], [EvaluatedValue])
getLetArgs' reg (ids, values) (Node [Leaf (Symbol varName), value]) = (RegisterId varName : ids, evaluateValue (Context (reg, value)) : values)
getLetArgs' _   _             _                                     = throw $ InvalidSyntax "missing bound value definition"
