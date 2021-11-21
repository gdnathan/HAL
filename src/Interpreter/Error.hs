--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Error
--

module Interpreter.Error  ( Error(..)
                          , BuiltInError(..)
                          ) where

import GHC.Exception ( Exception )

type ProbableReason = String
type ExpressionName = String
type Name           = String
type BuiltInName    = String

data BuiltInError = ArgumentIsNotAPair
instance Show BuiltInError where
  show ArgumentIsNotAPair = "attempt to apply non-pair"

data Error = InternalError        ProbableReason
           | UnknownName          Name
           | InvalidSyntax        ProbableReason
           | InvalidNumberOfArguments
           | BuiltInError BuiltInName BuiltInError
           | NotAProcedure
           | ArgumentIsNotNumber
instance Show Error where
  show (InternalError       _)    = "an unexpected error happened"
  show (UnknownName         name) = "variable " ++ name ++ " is not bound"
  show (InvalidSyntax       _)    = "invalid syntax"
  show InvalidNumberOfArguments   = "incorrect argument count"
  show NotAProcedure              = "attempt to apply non-procedure"
  show ArgumentIsNotNumber        = "attempt to apply non-number"
  show (BuiltInError  name error) = name ++ ": " ++ show error

instance Exception Error
