--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- EvaluateValue
--

module Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext(..)
                                  ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error( NotAProcedure ) )
import Interpreter.Parser         ( Tree(..)
                                  , ProcedureArg(..)
                                  )
import Interpreter.Register       ( regLookup
                                  , EvaluatedValue(..)
                                  , Register
                                  , RegisterId( RegisterId )
                                  )
import Interpreter.Builtins.Quote ( createList )

newtype EvaluatingContext = Context (Register, Tree)

evaluateValue :: EvaluatingContext -> EvaluatedValue
evaluateValue (Context (reg, Leaf arg))                         = evaluateNonProcedure reg arg
evaluateValue (Context (reg, Node (Leaf (Symbol rId)  : args)))  = callProcedure reg args $ regLookup reg $ RegisterId rId
evaluateValue (Context (reg, Node (rId@(Node _)       : args)))  = callProcedure reg args $ evaluateValue $ Context (reg, rId)
evaluateValue _                                                 = throw NotAProcedure

evaluateNonProcedure :: Register -> ProcedureArg -> EvaluatedValue
evaluateNonProcedure _   (Number        n)     = ValueNumber n
evaluateNonProcedure _   (Symbol        "#t")  = ValueTrue
evaluateNonProcedure _   (Symbol        "#f")  = ValueFalse
evaluateNonProcedure reg (Symbol        name)  = regLookup reg $ RegisterId name
evaluateNonProcedure _   (UncreatedList list)  = createList list

callProcedure :: Register -> [Tree] -> EvaluatedValue -> EvaluatedValue
callProcedure reg args (Procedure body) = body reg args
callProcedure reg args (ValueName name) = callProcedure reg args $ regLookup reg $ RegisterId name
callProcedure _   _    _                = throw NotAProcedure
