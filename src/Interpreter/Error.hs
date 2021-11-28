--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Error
--

module Interpreter.Error  ( Error(..)
                          , BuiltInError(..)
                          ) where

import GHC.Exception ( Exception )

type ProbableReason = String
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
           | DividingByZero
instance Show Error where
  show (InternalError       _)          = "an unexpected error happened"
  show (UnknownName         name)       = "variable " ++ name ++ " is not bound"
  show (InvalidSyntax       _)          = "invalid syntax"
  show InvalidNumberOfArguments         = "incorrect argument count"
  show NotAProcedure                    = "attempt to apply non-procedure"
  show ArgumentIsNotNumber              = "attempt to apply non-number"
  show (BuiltInError  name errorMsg)    = name ++ ": " ++ show errorMsg
  show DividingByZero                   = "division by zero is not allowed"

instance Exception Error
