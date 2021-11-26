--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- List
--

module Interpreter.Builtins.List  ( cons
                                  , car
                                  , cdr
                                  ) where

import Control.Exception          ( throw )

import Interpreter.Error          ( Error ( InvalidNumberOfArguments
                                          , BuiltInError
                                          )
                                  , BuiltInError ( ArgumentIsNotAPair )
                                  )
import Interpreter.Data.Register  ( Register
                                  , EvaluatedValue( List )
                                  )
import Interpreter.Parser         ( Tree )
import Interpreter.EvaluateValue  ( evaluateValue
                                  , EvaluatingContext( Context )
                                  )

--- Builtin ---
cons :: Register -> [Tree] -> EvaluatedValue
cons reg [left, right]  = List (evaluateValue $ Context (reg, left), evaluateValue $ Context (reg, right))
cons _   _              = throw InvalidNumberOfArguments

--- Builtin ---
car :: Register -> [Tree] -> EvaluatedValue
car reg [list]  = car' $ evaluateValue $ Context (reg, list)
car _   _       = throw InvalidNumberOfArguments

car' :: EvaluatedValue -> EvaluatedValue
car' (List (left, _)) = left
car' _                = throw $ BuiltInError "cdr" ArgumentIsNotAPair

--- Builtin ---
cdr :: Register -> [Tree] -> EvaluatedValue
cdr reg [list]  = cdr' $ evaluateValue $ Context (reg, list)
cdr _   _       = throw InvalidNumberOfArguments

cdr' :: EvaluatedValue -> EvaluatedValue
cdr' (List (_, right))  = right
cdr' _                  = throw $ BuiltInError "cdr" ArgumentIsNotAPair
