--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Error
--

module CLIArguments.Error   ( Error(..) ) where

import GHC.Exception ( Exception )

type OptionName = String

data Error = ArgumentParsingError String
           | InvalidOption OptionName
instance Show Error where
  show (InvalidOption         optionName) = "Invalid option '" ++ optionName ++ "'"
  show (ArgumentParsingError  str)        = str
instance Exception Error
