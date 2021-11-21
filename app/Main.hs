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
import Interpreter.Lexer ( tokenize )    -- ( buildEvaluationTree, HalExecution(..) )
import Interpreter.Parser ( buildExpressionsTrees )   -- ( buildEvaluationTree, HalExecution(..) )
-- import Interpreter.Evaluate (EvaluatedValue(..), EvaluatingContext(..), EvaluationResult(..), Register(..), evaluateExpr)
import Interpreter.Data.Tree ( Tree(..) )
import Interpreter.EvaluateValue ( EvaluatingContext(..), evaluateValue)
import Interpreter.EvaluateExpr ( EvaluationResult(..), evaluateExpr)
import Interpreter.Data.Register    ( Register
                                    , EvaluatedValue(..)
                                    , regInsertRange2
                                    , RegisterId (RegisterId)
                                    )
import Interpreter.Builtins.All (initialRegister)
import System.IO (hFlush, stdout)
-- import Error                ( Error(..) )

main :: IO ()
main = catches
        (getArgs >>= dispatchExecutions . parseArgs)
        [ Handler handleCLIArgumentsErrors
        -- , Handler handleErrors
        ]

-- type Context = Int

dispatchExecutions :: HalExecution -> IO ()
dispatchExecutions execution = dispatchExecutions' execution initialRegister

dispatchExecutions' :: HalExecution -> Register -> IO ()
dispatchExecutions' PrintHelp               _       = printHelp
dispatchExecutions' (Evaluate []          ) _       = throw $ ArgumentParsingError "Nothing to evaluate."
dispatchExecutions' (Evaluate [file]      ) reg = readFile file >>= \content -> print $ resVal $ evaluateFile reg content
dispatchExecutions' (Evaluate (file : xs) ) reg = readFile file >>= \content -> dispatchExecutions' (Evaluate xs) $ resReg $ evaluateFile reg content
dispatchExecutions' (Repl     []          ) reg = infiniteLoop reg
dispatchExecutions' (Repl     (file : xs) ) reg = readFile file >>= \content -> dispatchExecutions' (Repl xs) $ resReg $ evaluateFile reg content

printHelp :: IO ()
printHelp = putStrLn "This is a help."

-- emptyContext :: EvaluatingContext
-- emptyContext = Context (initialRegister, Empty)
-- (cond (#t 42))
infiniteLoop :: Register -> IO ()
infiniteLoop reg = putStr "> " >> hFlush stdout >> (getLine >>= \line -> infiniteLoop' reg line)

infiniteLoop' :: Register -> String -> IO ()
infiniteLoop' reg line = let (Result (newReg, res)) = evaluateFile reg line in print res >> infiniteLoop newReg

-- type EvaluatedValue = [Tree]
evaluateFile :: Register -> String -> EvaluationResult
evaluateFile reg str = evaluateTrees reg $ buildExpressionsTrees $ tokenize str

evaluateTrees :: Register -> [Tree] -> EvaluationResult
evaluateTrees reg = evaluateTrees' (Result (reg, NoValue))

evaluateTrees' :: EvaluationResult -> [Tree] -> EvaluationResult
evaluateTrees' res               []               = res
evaluateTrees' (Result (reg, _)) (tree : others)  = evaluateTrees' (evaluateExpr (Context (reg, tree))) others

resReg :: EvaluationResult -> Register
resReg (Result (reg, _)) = reg

resVal :: EvaluationResult -> EvaluatedValue
resVal (Result (_, val)) = val

handleCLIArgumentsErrors :: CLIArguments.Error.Error -> IO ()
handleCLIArgumentsErrors (InvalidOption optionName) = putStrLn ("Invalid option '" ++ optionName ++ "'")  >> exitWith (ExitFailure 84)
handleCLIArgumentsErrors (ArgumentParsingError str) = putStrLn str                                        >> exitWith (ExitFailure 84)

-- handleErrors :: Error.Error -> IO ()
-- handleErrors HelpError = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
-- handleErrors _ = putStrLn "Invalid option" >> exitWith (ExitFailure 84)
