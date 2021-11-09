--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Error
--

module Interpreter.Error   ( Error(..) ) where

import GHC.Exception ( Exception )

type ExpressionName = String

data Error = ParsingError String
           | NameStartWithNumber ExpressionName
  deriving Show
instance Exception Error
