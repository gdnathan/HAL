--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments Error
--

module CLIArguments.Error ( Error(..) ) where

import GHC.Exception      ( Exception )

type OptionName = String

data Error = ArgumentParsingError String
           | InvalidOption OptionName
instance Show Error where
  show (InvalidOption         optionName) = "Invalid option '" ++ optionName ++ "'"
  show (ArgumentParsingError  str)        = str
instance Exception Error
