--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Define
--

module Interpreter.Builtins.Define where

import Data.Map                   ( Map )
import qualified Data.Map as Map
import Control.Exception          ( throw )

import Interpreter.Error          ( Error( InvalidNumberOfArguments, InvalidSyntax ) )
import Interpreter.Data.Register  ( regInsert
                                  , EvaluatedValue( Procedure )
                                  , Register(..)
                                  , RegisterId(..)
                                  )
import Interpreter.Data.Tree      ( Tree(..)
                                  , ProcedureArg( Symbol )
                                  )
import Interpreter.EvaluateValue  ( EvaluatingContext( Context )
                                  , evaluateValue
                                  )

define :: Register -> [Tree] -> (Register, String)
define reg  [Leaf (Symbol name)                  , body] = (regInsert reg (RegisterId name) $ evaluateValue (Context (reg, body)) , name)
define reg  [Node (Leaf (Symbol name) : argsName), body] = (regInsert reg (RegisterId name) $ createProcedure argsName body       , name)
define _               _                                 = throw $ InvalidSyntax "Invalid define definition"

createProcedure :: [Tree] -> Tree -> EvaluatedValue
createProcedure = createProcedure' evaluateArgs

createProcedure' :: (Register -> [Tree] -> [EvaluatedValue]) -> [Tree] -> Tree -> EvaluatedValue
createProcedure' evalArgs argsName body = Procedure $ \reg args -> evaluateValue $ Context (addArgsToRegister (argsToIdsList argsName) (evalArgs reg args) reg, body)

evaluateArgs :: Register -> [Tree] -> [EvaluatedValue]
evaluateArgs _    []        = []
evaluateArgs reg  (x : xs)  = evaluateValue (Context (reg, x)) : evaluateArgs reg xs

argsToIdsList :: [Tree] -> [RegisterId]
argsToIdsList []                          = []
argsToIdsList ((Leaf (Symbol name)) : xs) = RegisterId name : argsToIdsList xs
argsToIdsList _                           = throw $ InvalidSyntax "argument name must be a string"

addArgsToRegister :: [RegisterId] -> [EvaluatedValue] -> Register -> Register
addArgsToRegister []        []        reg             = reg
addArgsToRegister _         []        _               = throw InvalidNumberOfArguments
addArgsToRegister []        _         _               = throw InvalidNumberOfArguments
addArgsToRegister (x : xs)  (y : ys)  (Register reg)  = addArgsToRegister xs ys $ Register $ Map.insert x y reg
