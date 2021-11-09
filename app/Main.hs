--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Main
--

module Main where

import System.Environment               ( getArgs )
import Control.Exception                ( catches, Handler(..), throw )
import System.Exit                      ( ExitCode( ExitFailure )
                                        , exitWith )

import CLIArguments.Parser  ( parseArgs, HalExecution(..) )
import CLIArguments.Error   ( Error(..) )
import System.IO (readFile)
import GHC.IO (evaluate)
-- import Error                ( Error(..) )

main :: IO ()
main = catches
        (getArgs >>= dispatchExecutions . parseArgs)
        [ Handler handleCLIArgumentsErrors
        -- , Handler handleErrors
        ]

type Context = Int

dispatchExecutions :: HalExecution -> IO ()
dispatchExecutions execution = dispatchExecutions' execution emptyContext

dispatchExecutions' :: HalExecution -> Context -> IO ()
dispatchExecutions' PrintHelp               context = printHelp
dispatchExecutions' (Evaluate []          ) context = throw $ ArgumentParsingError "Nothing to evaluate."
dispatchExecutions' (Evaluate [file]      ) context = readFile file >>= \content -> print $ snd $ evaluateExpr context content
dispatchExecutions' (Evaluate (file : xs) ) context = readFile file >>= \content -> dispatchExecutions' (Evaluate xs) $ fst $ evaluateExpr context content
dispatchExecutions' (Repl     []          ) context = infiniteLoop context
dispatchExecutions' (Repl     (file : xs) ) context = readFile file >>= \content -> dispatchExecutions' (Repl xs) $ fst $ evaluateExpr context content

printHelp :: IO ()
printHelp = putStrLn "This is a help."

emptyContext :: Context
emptyContext = 2

infiniteLoop :: Context -> IO ()
infiniteLoop _ = putStrLn "Infinite loop..."

type EvaluatedValue = Int
evaluateExpr :: Context -> String -> (EvaluatedValue, Context)
evaluateExpr _ _ = (1, 2)

handleCLIArgumentsErrors :: CLIArguments.Error.Error -> IO ()
handleCLIArgumentsErrors (InvalidOption optionName) = putStrLn ("Invalid option '" ++ optionName ++ "'")  >> exitWith (ExitFailure 84)
handleCLIArgumentsErrors (ArgumentParsingError str) = putStrLn str                                        >> exitWith (ExitFailure 84)

-- handleErrors :: Error.Error -> IO ()
-- handleErrors HelpError = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
-- handleErrors _ = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
