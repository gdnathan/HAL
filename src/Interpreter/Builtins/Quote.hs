--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Quote
--

module Interpreter.Builtins.Quote ( quote ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments ) )
import Interpreter.Data.Register  ( Register
                                  , EvaluatedValue( ValueNil
                                                  , ValueNumber
                                                  , ValueName
                                                  , List
                                                  )
                                  )
import Interpreter.Data.Tree      ( Tree ( Node, Leaf )
                                  , ProcedureArg  ( Symbol
                                                  , Number
                                                  , UnCreatedList
                                                  )
                                  )
import Interpreter.EvaluateValue  ( createList )

--- Builtin ---
quote :: Register -> [Tree] -> EvaluatedValue
quote reg [Node []]                   = ValueNil
quote reg [Node args]                 = createList reg args
quote reg [Leaf (Number n)]           = ValueNumber n
quote reg [Leaf (Symbol name)]        = ValueName name
quote reg [Leaf (UnCreatedList list)] = List (ValueName "quote", createList reg list)
quote _    _                          = throw InvalidNumberOfArguments
