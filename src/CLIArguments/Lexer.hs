--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Lexer
--

module CLIArguments.Lexer   ( Token(..)
                            , tokenizeArgs
                            ) where

import Control.Exception    ( throw )

import CLIArguments.Error   ( Error( InvalidOption ) )

data Token = HELP
           | LAUNCH_REPL
           | FILE String

tokenizeArgs :: [String] -> [Token]
tokenizeArgs []            = []
tokenizeArgs (('-' : opt) : xs) = tokenizeOption opt : tokenizeArgs xs
tokenizeArgs (x           : xs) = FILE x : tokenizeArgs xs

tokenizeOption :: String -> Token
tokenizeOption ('-':xs)   = tokenizeLongOption xs
tokenizeOption "h"        = HELP
tokenizeOption "i"        = LAUNCH_REPL
tokenizeOption xs         = throw $ InvalidOption xs

tokenizeLongOption :: String -> Token
tokenizeLongOption "help" = HELP
tokenizeLongOption "repl" = LAUNCH_REPL
tokenizeLongOption xs     = throw $ InvalidOption xs
