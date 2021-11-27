--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Quote
--

module Interpreter.Builtins.Quote ( quote, createList ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments ) )
import Interpreter.Register       ( Register
                                  , EvaluatedValue( ValueNil
                                                  , ValueNumber
                                                  , ValueName
                                                  , List
                                                  )
                                  )
import Interpreter.Parser         ( Tree ( Node, Leaf )
                                  , ProcedureArg  ( Symbol
                                                  , Number
                                                  , UncreatedList
                                                  )
                                  )

--- Builtin ---
quote :: Register -> [Tree] -> EvaluatedValue
quote _ = quote'

quote' :: [Tree] -> EvaluatedValue
quote' [Node []]                   = ValueNil
quote' [Node args]                 = createList args
quote' [Leaf (UncreatedList list)] = List (ValueName "quote", List (createList list, ValueNil))
quote' [Leaf (Number n)]           = ValueNumber n
quote' [Leaf (Symbol n)]           = ValueName n
quote'  args                       = throw InvalidNumberOfArguments

createList :: [Tree] -> EvaluatedValue
createList []          = ValueNil
createList (left : xs) = List (quote' [left], createList xs)
