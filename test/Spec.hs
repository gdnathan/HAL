--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Spec
--

module Main                           ( main ) where

import Test.Hspec                     ( hspec
                                      , Spec
                                      , describe
                                      )

import CLIArguments.LexerSpec         ( spec )
import CLIArguments.ParserSpec        ( spec )
import Interpreter.LexerSpec          ( spec )
import Interpreter.ParserSpec         ( spec )
import Interpreter.EvaluateExprSpec   ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "CLI Arguments LexerSpec"        CLIArguments.LexerSpec.spec
    describe "CLI Arguments ParserSpec"       CLIArguments.ParserSpec.spec
    describe "Interpreter LexerSpec"          Interpreter.LexerSpec.spec
    describe "Interpreter ParserSpec"         Interpreter.ParserSpec.spec
    describe "Interpreter EvaluateExprSpec"   Interpreter.EvaluateExprSpec.spec
