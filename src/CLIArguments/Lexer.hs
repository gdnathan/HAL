--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments Lexer
--

module CLIArguments.Lexer   ( Token(..)
                            , tokenize
                            ) where

import Control.Exception    ( throw )

import CLIArguments.Error   ( Error( InvalidOption ) )

data Token = Help
           | LaunchRepl
           | File String

tokenize :: [String] -> [Token]
tokenize []                 = []
tokenize (('-' : opt) : xs) = tokenizeOption opt : tokenize xs
tokenize (x           : xs) = File x             : tokenize xs

tokenizeOption :: String -> Token
tokenizeOption ('-' : xs) = tokenizeLongOption xs
tokenizeOption "h"        = Help
tokenizeOption "i"        = LaunchRepl
tokenizeOption xs         = throw $ InvalidOption xs

tokenizeLongOption :: String -> Token
tokenizeLongOption "help" = Help
tokenizeLongOption "repl" = LaunchRepl
tokenizeLongOption xs     = throw $ InvalidOption xs
