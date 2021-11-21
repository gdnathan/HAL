--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Parser
--

module CLIArguments.Parser    ( parseArgs
                              , HalConfig(..)
                              , HalExecution(..)
                              ) where

import CLIArguments.Lexer     ( Token(..), tokenizeArgs )

data HalExecution = PrintHelp | Repl | Evaluate
  deriving Show

data HalConfig = HalConfig HalExecution [String]
  deriving Show

parseArgs :: [String] -> HalConfig
parseArgs = parseTokenizedArgs . tokenizeArgs

parseTokenizedArgs :: [Token] -> HalConfig
parseTokenizedArgs []     = defaultHalExecution
parseTokenizedArgs tokens = foldr parseTokenizedArgs' (HalConfig Evaluate []) tokens

parseTokenizedArgs' :: Token -> HalConfig -> HalConfig
parseTokenizedArgs' HELP        _                               = HalConfig PrintHelp []
parseTokenizedArgs' _           (HalConfig PrintHelp _)         = HalConfig PrintHelp []
parseTokenizedArgs' LAUNCH_REPL (HalConfig _         filesName) = HalConfig Repl filesName
parseTokenizedArgs' (FILE file) (HalConfig exec      filesName) = HalConfig exec  $ file : filesName

defaultHalExecution :: HalConfig
defaultHalExecution = HalConfig Repl []
