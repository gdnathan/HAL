--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments Lexer
--

module CLIArguments.Lexer   ( Token(..)
                            , tokenizeArgs
                            ) where

import Control.Exception    ( throw )

import CLIArguments.Error   ( Error( InvalidOption ) )

data Token = Help
           | LaunchRepl
           | File String

tokenizeArgs :: [String] -> [Token]
tokenizeArgs []                 = []
tokenizeArgs (('-' : opt) : xs) = tokenizeOption opt : tokenizeArgs xs
tokenizeArgs (x           : xs) = File x             : tokenizeArgs xs

tokenizeOption :: String -> Token
tokenizeOption ('-' : xs) = tokenizeLongOption xs
tokenizeOption "h"        = Help
tokenizeOption "i"        = LaunchRepl
tokenizeOption xs         = throw $ InvalidOption xs

tokenizeLongOption :: String -> Token
tokenizeLongOption "help" = Help
tokenizeLongOption "repl" = LaunchRepl
tokenizeLongOption xs     = throw $ InvalidOption xs
