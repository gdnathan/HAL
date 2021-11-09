--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Parser
--

module CLIArguments.Parser    ( HalExecution(..)
                              , parseArgs
                              ) where

import CLIArguments.Lexer     ( Token(..), tokenizeArgs )

data HalExecution = PrintHelp | Repl [String] | Evaluate [String]
  deriving Show

parseArgs :: [String] -> HalExecution
parseArgs = parseTokenizedArgs . tokenizeArgs

parseTokenizedArgs :: [Token] -> HalExecution
parseTokenizedArgs []     = defaultHalExecution
parseTokenizedArgs tokens = foldr parseTokenizedArgs' (Evaluate []) tokens

parseTokenizedArgs' :: Token -> HalExecution -> HalExecution
parseTokenizedArgs' _             PrintHelp               = PrintHelp
parseTokenizedArgs' HELP          _                       = PrintHelp
parseTokenizedArgs' LAUNCH_REPL   (Evaluate   filesName)  = Repl filesName
parseTokenizedArgs' LAUNCH_REPL   conf@(Repl  _        )  = conf
parseTokenizedArgs' (FILE file)   (Evaluate   filesName)  = Evaluate  $ file : filesName
parseTokenizedArgs' (FILE file)   (Repl       filesName)  = Repl      $ file : filesName

defaultHalExecution :: HalExecution
defaultHalExecution = Repl []
