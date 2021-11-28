--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments Parser
--

module CLIArguments.Parser  ( parseArgs
                            , HalConfig(..)
                            , HalExecution(..)
                            ) where

import CLIArguments.Lexer   ( Token(..)
                            , tokenize
                            )

data HalExecution = PrintHelp
                  | Repl
                  | Evaluate

data HalConfig = HalConfig HalExecution [String]

parseArgs :: [String] -> HalConfig
parseArgs = parseTokenizedArgs . tokenize

parseTokenizedArgs :: [Token] -> HalConfig
parseTokenizedArgs []     = defaultHalConfig
parseTokenizedArgs tokens = foldr parseTokenizedArgs' (HalConfig Evaluate []) tokens

parseTokenizedArgs' :: Token -> HalConfig -> HalConfig
parseTokenizedArgs' Help        _                               = HalConfig PrintHelp []
parseTokenizedArgs' _           (HalConfig PrintHelp _)         = HalConfig PrintHelp []
parseTokenizedArgs' LaunchRepl  (HalConfig _         fileNames) = HalConfig Repl fileNames
parseTokenizedArgs' (File file) (HalConfig halExec   fileNames) = HalConfig halExec $ file : fileNames

defaultHalConfig :: HalConfig
defaultHalConfig = HalConfig Repl []
