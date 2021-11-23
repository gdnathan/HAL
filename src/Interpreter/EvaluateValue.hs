--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- EvaluateValue
--

module Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext(..)
                                  , createList
                                  ) where

import Data.Map                   ( Map, fromList )
import qualified Data.Map as Map
import Control.Exception          ( throw )

import Interpreter.Data.Tree      ( Tree (..)
                                  , ProcedureArg (..)
                                  )
import Interpreter.Data.Register  ( Register(..)
                                  , RegisterId (..)
                                  , EvaluatedValue(..)
                                  , regLookup
                                  , regLookupMaybe
                                  )
import Interpreter.Error          ( Error(..) )

newtype EvaluatingContext = Context (Register, Tree)
  deriving (Show)

evaluateValue :: EvaluatingContext -> EvaluatedValue
evaluateValue (Context (reg, Leaf    arg))                        = evaluateNonProcedure reg arg
evaluateValue (Context (reg, Node    (Leaf (Symbol id)  : args))) = callProcedure reg (regLookup reg (RegisterId id))     args
evaluateValue (Context (reg, Node    (id@(Node _)       : args))) = callProcedure reg (evaluateValue (Context (reg, id))) args
evaluateValue _                                                   = throw NotAProcedure

evaluateNonProcedure :: Register -> ProcedureArg -> EvaluatedValue
evaluateNonProcedure _   (Number   n)          = ValueNumber n
evaluateNonProcedure _   (Symbol   "#t")       = ValueTrue
evaluateNonProcedure _   (Symbol   "#f")       = ValueFalse
evaluateNonProcedure reg (Symbol   name)       = evaluateNonProcedure' name $ regLookupMaybe reg (RegisterId name)
evaluateNonProcedure reg (UnCreatedList list)  = createList reg list

evaluateNonProcedure' :: String -> Maybe EvaluatedValue -> EvaluatedValue
evaluateNonProcedure' name  Nothing       = ValueName name
evaluateNonProcedure' _     (Just value)  = value

createList :: Register -> [Tree] -> EvaluatedValue
createList _    []          = ValueNil
createList reg  (left : xs) = List (evaluateValue (Context (reg, left)), createList reg xs)

callProcedure :: Register -> EvaluatedValue -> [Tree] -> EvaluatedValue
callProcedure reg (Procedure    body) args  = body reg args
callProcedure reg (ValueName    name) args  = callProcedure reg (regLookup reg (RegisterId name)) args
callProcedure _   _                   _     = throw NotAProcedure
