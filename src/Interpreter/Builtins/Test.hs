--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Test
--

module Interpreter.Builtins.Test  ( isEq
                                  , isAtom
                                  , cond
                                  ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments
                                          , NotAProcedure
                                          , InvalidSyntax
                                          )
                                  )
import Interpreter.Data.Register  ( Register
                                  , EvaluatedValue( List
                                                  , ValueFalse
                                                  , ValueTrue
                                                  , ValueName
                                                  , NoValue
                                                  )
                                  )
import Interpreter.Data.Tree      ( Tree ( Node, Leaf )
                                  , ProcedureArg ( Symbol )
                                  )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )

--- Builtin ---
isEq :: Register -> [Tree] -> EvaluatedValue
isEq reg [left, right]
  | evaluateValue (Context (reg, left)) == evaluateValue (Context (reg, right)) = ValueTrue
  | otherwise                                                                   = ValueFalse
isEq _    _              = throw InvalidNumberOfArguments

--- Builtin ---
isAtom :: Register -> [Tree] -> EvaluatedValue
isAtom reg [arg] = isAtom' $ evaluateValue (Context (reg, arg))
isAtom _    _    = throw InvalidNumberOfArguments

isAtom' :: EvaluatedValue -> EvaluatedValue
isAtom' (List _)  = ValueFalse
isAtom' _         = ValueTrue

--- Builtin ---
cond :: Register -> [Tree] -> EvaluatedValue
cond _   []   = throw InvalidNumberOfArguments
cond reg args = cond' reg args

cond' :: Register -> [Tree] -> EvaluatedValue
cond' reg (Node (Leaf (Symbol "#f") : _)    : xs) = cond' reg xs
cond' reg (Node [Leaf _             , arg]  : _)  = evaluateValue $ Context (reg, arg)
cond' reg (Node [id@(Node _)        , arg]  : xs) = cond' reg (Node [Leaf $ Symbol $ evaluateId reg id, arg] : xs)
cond' reg (Node [toReturn]                  : xs) = evaluateValue $ Context (reg, toReturn)
cond' reg (Node (head : middle : end)       : xs) = cond' reg $ Node (head : end) : xs
cond' _   []                                      = NoValue
cond' reg (Node []                          : xs) = throw $ InvalidSyntax "must have at least one value to return"
cond' _   (Leaf _                           : _)  = throw $ InvalidSyntax "argument(s) must be an expression"

evaluateId :: Register -> Tree -> String
evaluateId reg node = evaluateId' $ evaluateValue $ Context (reg, node)

evaluateId' :: EvaluatedValue -> String
evaluateId' (ValueName str)  = str
evaluateId' ValueFalse       = "#f"
evaluateId' ValueTrue        = "#t"
evaluateId' _                = throw NotAProcedure
