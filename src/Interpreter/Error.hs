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
  deriving Show

data Error = ParsingError         ProbableReason
           | NameStartWithNumber  ExpressionName
           | InternalError        ProbableReason
           | UnknownName          Name
           | InvalidSyntax        ProbableReason
           | InvalidNumberOfArguments
           | BuiltInError BuiltInName BuiltInError
           | NotAProcedure
           | ArgumentIsNotNumber
  deriving Show
instance Exception Error
