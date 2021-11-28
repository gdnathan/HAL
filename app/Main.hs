--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Main
--

module Main where

import System.Environment         ( getArgs )
import Control.Exception          ( catches, throw, Handler(..) )
import System.Exit                ( exitWith, ExitCode( ExitFailure ) )
import System.IO                  ( hFlush, stdout )

import CLIArguments.Parser        ( parseArgs
                                  , HalConfig(..)
                                  , HalExecution( PrintHelp, Evaluate, Repl )
                                  )
import CLIArguments.Error         ( Error( ArgumentParsingError ) )
import Interpreter.Error          ( Error )
import Interpreter.EvaluateExpr   ( evaluateExpr, EvaluationResult(..) )
import Interpreter.Register       ( EvaluatedValue, Register )
import Interpreter.Builtins.All   ( initialRegister )

main :: IO ()
main = catches
        (getArgs >>= dispatchExecutions . replaceFileNameByFileContent . parseArgs)
        [ Handler handleCLIArgumentsErrors
        , Handler handleInterpreterErrors
        ]

replaceFileNameByFileContent :: HalConfig -> IO HalConfig
replaceFileNameByFileContent (HalConfig PrintHelp _)     = return $ HalConfig PrintHelp []
replaceFileNameByFileContent (HalConfig exec      files) = readFileList files >>= \contents -> return $ HalConfig exec contents

readFileList :: [String] -> IO [String]
readFileList []           = return []
readFileList (file : xs)  = readFile file >>= \content -> readFileList xs >>= \res -> return (content : res)

dispatchExecutions :: IO HalConfig -> IO ()
dispatchExecutions config = config >>= \conf -> dispatchExecutions' conf initialRegister

dispatchExecutions' :: HalConfig -> Register -> IO ()
dispatchExecutions' (HalConfig PrintHelp  _)                  _   = printHelp
dispatchExecutions' (HalConfig Evaluate   [fileContent])      reg = print $ resVal $ evaluateExpr reg fileContent
dispatchExecutions' (HalConfig exec       (fileContent : xs)) reg = dispatchExecutions' (HalConfig exec xs) $ resReg $ evaluateExpr reg fileContent
dispatchExecutions' (HalConfig Repl       [])                 reg = infiniteLoop reg
dispatchExecutions' (HalConfig Evaluate   [])                 _   = throw $ ArgumentParsingError "nothing to evaluate."

resVal :: EvaluationResult -> EvaluatedValue
resVal (Result (_, val)) = val

resReg :: EvaluationResult -> Register
resReg (Result (reg, _)) = reg

printHelp :: IO ()
printHelp = putStrLn  "./hal [files | flags] ...\
                      \ (none)  -> launch the REPL\
                      \ files   -> a list of files to be interpreted\
                      \ flags   ->\
                      \     -h | --help  -> display this help\
                      \     -i           -> launch the REPL"

infiniteLoop :: Register -> IO ()
infiniteLoop reg = putStr "> " >> hFlush stdout >> (getLine >>= \line -> infiniteLoop' $ evaluateExpr reg line)

infiniteLoop' :: EvaluationResult -> IO ()
infiniteLoop' (Result (newReg, res)) = print res >> infiniteLoop newReg

handleCLIArgumentsErrors :: CLIArguments.Error.Error -> IO ()
handleCLIArgumentsErrors errorType = putStrLn ("CLI Exception: " ++ show errorType) >> exitWith (ExitFailure 84)

handleInterpreterErrors :: Interpreter.Error.Error -> IO ()
handleInterpreterErrors errorType = putStrLn ("Exception: " ++ show errorType) >> exitWith (ExitFailure 84)
