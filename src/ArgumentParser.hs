module ArgumentParser         ( HalEvaluationMode(..)
                              , HalArgs(..)
                              , parseArgs
                              ) where

import Control.Exception.Base ( throw )

import ArgumentLexer          ( Token(..), tokenizeArgs )
import Error                  ( Error(..) )

data HalEvaluationMode = EVALUATE | REPL

data HalArgs = HalArgs HalEvaluationMode (Maybe [String])

parseArgs :: [String] -> HalArgs
parseArgs = parseTokenizedArgs . tokenizeArgs

parseTokenizedArgs :: [Token] -> HalArgs
parseTokenizedArgs []     = defaultHalArgs
parseTokenizedArgs tokens = parseTokenizedArgs' tokens (HalArgs EVALUATE Nothing)

parseTokenizedArgs' :: [Token] -> HalArgs -> HalArgs
parseTokenizedArgs' []               (HalArgs evaluation_mode (Just files)) = HalArgs evaluation_mode $ Just (reverse files)
parseTokenizedArgs' []               (HalArgs EVALUATE Nothing)             = throw $ ArgumentParsingError "There is nothing to evaluate."
parseTokenizedArgs' []               config@(HalArgs REPL Nothing)          = config 
parseTokenizedArgs' (HELP:_)         _                                      = throw HelpError
parseTokenizedArgs' (LAUNCH_REPL:xs) (HalArgs _ files)                      = parseTokenizedArgs' xs $ HalArgs REPL files
parseTokenizedArgs' (FILE x:xs)      (HalArgs evaluation_mode (Just files)) = parseTokenizedArgs' xs $ HalArgs evaluation_mode (Just (x:files))
parseTokenizedArgs' (FILE x:xs)      (HalArgs evaluation_mode Nothing)      = parseTokenizedArgs' xs $ HalArgs evaluation_mode (Just [x])

defaultHalArgs :: HalArgs
defaultHalArgs = HalArgs REPL Nothing
