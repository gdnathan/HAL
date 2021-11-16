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
import Interpreter.Lexer    -- ( buildEvaluationTree, HalExecution(..) )
import Interpreter.Parser   -- ( buildEvaluationTree, HalExecution(..) )
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
dispatchExecutions' PrintHelp               _       = printHelp
dispatchExecutions' (Evaluate []          ) _       = throw $ ArgumentParsingError "Nothing to evaluate."
dispatchExecutions' (Evaluate [file]      ) context = readFile file >>= \content -> print $ fst $ evaluateExpr context content
dispatchExecutions' (Evaluate (file : xs) ) context = readFile file >>= \content -> dispatchExecutions' (Evaluate xs) $ snd $ evaluateExpr context content
dispatchExecutions' (Repl     []          ) context = infiniteLoop context
dispatchExecutions' (Repl     (file : xs) ) context = readFile file >>= \content -> dispatchExecutions' (Repl xs) $ snd $ evaluateExpr context content

printHelp :: IO ()
printHelp = putStrLn "This is a help."

emptyContext :: Context
emptyContext = 2

infiniteLoop :: Context -> IO ()
infiniteLoop _ = putStrLn "Infinite loop..."

type EvaluatedValue = [Tree]
evaluateExpr :: Context -> String -> (EvaluatedValue, Context)
evaluateExpr _ str = (buildExpressionsTrees $ tokenize str, 2)

handleCLIArgumentsErrors :: CLIArguments.Error.Error -> IO ()
handleCLIArgumentsErrors (InvalidOption optionName) = putStrLn ("Invalid option '" ++ optionName ++ "'")  >> exitWith (ExitFailure 84)
handleCLIArgumentsErrors (ArgumentParsingError str) = putStrLn str                                        >> exitWith (ExitFailure 84)

-- handleErrors :: Error.Error -> IO ()
-- handleErrors HelpError = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
-- handleErrors _ = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
