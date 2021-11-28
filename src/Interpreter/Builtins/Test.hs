--
-- EPITECH PROJECT, 2021
-- HAL
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
import Interpreter.Register       ( Register
                                  , EvaluatedValue( List
                                                  , ValueFalse
                                                  , ValueTrue
                                                  , ValueName
                                                  , NoValue, ValueNumber, ValueNil
                                                  ), regLookup, RegisterId (RegisterId)
                                  )
import Interpreter.Parser         ( Tree ( Node, Leaf )
                                  , ProcedureArg ( Symbol )
                                  )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )

--- Builtin ---
isEq :: Register -> [Tree] -> EvaluatedValue
isEq reg [left, right]
  | evaluateValue (Context (reg, left)) `isEq'` evaluateValue (Context (reg, right))  = ValueTrue
  | otherwise                                                                         = ValueFalse
isEq _   _                                                                            = throw InvalidNumberOfArguments

isEq' :: EvaluatedValue -> EvaluatedValue -> Bool
isEq' (ValueNumber x) (ValueNumber y)  = x == y
isEq' (ValueName   x) (ValueName   y)  = x == y
isEq'  ValueNil        ValueNil        = True
isEq'  _               _               = False

--- Builtin ---
isAtom :: Register -> [Tree] -> EvaluatedValue
isAtom reg [arg] = isAtom' $ evaluateValue (Context (reg, arg))
isAtom _   _     = throw InvalidNumberOfArguments

isAtom' :: EvaluatedValue -> EvaluatedValue
isAtom' (List _)  = ValueFalse
isAtom' _         = ValueTrue

--- Builtin ---
cond :: Register -> [Tree] -> EvaluatedValue
cond _   []   = throw InvalidNumberOfArguments
cond reg args = cond' reg args

cond' :: Register -> [Tree] -> EvaluatedValue
cond' reg (Node (head : values) : args) = cond'' (evaluateValue $ Context (reg, head)) values reg args
cond' _   []                            = NoValue
cond' reg (Node []              : xs)   = throw $ InvalidSyntax "must have at least one value to return"
cond' _   (Leaf _               : _)    = throw $ InvalidSyntax "argument(s) must be an expression"

cond'' :: EvaluatedValue -> [Tree] -> Register -> [Tree] -> EvaluatedValue
cond'' ValueFalse _      reg args = cond' reg args
cond'' headVal    values reg _    = foldl (\_ val -> evaluateValue $ Context (reg, val)) headVal values
