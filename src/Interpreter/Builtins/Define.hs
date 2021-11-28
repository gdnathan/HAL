--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Define
--

module Interpreter.Builtins.Define  ( define
                                    , createProcedure
                                    , createProcedure'
                                    ) where

import Control.Exception            ( throw )

import Interpreter.Error            ( Error( InvalidNumberOfArguments
                                            , InvalidSyntax
                                            )
                                    )
import Interpreter.Register         ( regInsert
                                    , regInsertRange
                                    , EvaluatedValue( Procedure )
                                    , Register
                                    , RegisterId( RegisterId )
                                    )
import Interpreter.Parser           ( Tree(..)
                                    , ProcedureArg( Symbol )
                                    )
import Interpreter.EvaluateValue    ( evaluateValue
                                    , EvaluatingContext( Context )
                                    )

define :: Register -> [Tree] -> (Register, RegisterId)
define reg  [Leaf (Symbol name)                  , body] = (regInsert reg (RegisterId name) $ evaluateValue (Context (reg, body)) , RegisterId name)
define reg  [Node (Leaf (Symbol name) : argsName), body] = (regInsert reg (RegisterId name) $ createProcedure argsName body       , RegisterId name)
define _               _                                 = throw $ InvalidSyntax "Invalid define definition"

createProcedure :: [Tree] -> Tree -> EvaluatedValue
createProcedure = createProcedure' evaluateArgs

createProcedure' :: (Register -> [Tree] -> [EvaluatedValue]) -> [Tree] -> Tree -> EvaluatedValue
createProcedure' prepArgs argsName body = Procedure $ \reg args -> evaluateValue $ Context (addArgsToRegister reg (map argToId argsName) (prepArgs reg args), body)

evaluateArgs :: Register -> [Tree] -> [EvaluatedValue]
evaluateArgs reg = map $ evaluateValue . curry Context reg

argToId :: Tree -> RegisterId
argToId (Leaf (Symbol name)) = RegisterId name
argToId _                    = throw $ InvalidSyntax "argument name must be a string"

addArgsToRegister :: Register -> [RegisterId] -> [EvaluatedValue] -> Register
addArgsToRegister reg ids values
    | length ids /= length values = throw InvalidNumberOfArguments
    | otherwise                   = regInsertRange reg ids values
