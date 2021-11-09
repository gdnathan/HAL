--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Error
--

module Error         ( Error(..) ) where

import GHC.Exception ( Exception )

data Error = HelpError
           | ArgumentParsingError String
             deriving Show
instance Exception Error