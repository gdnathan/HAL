module Main                     ( main ) where

import Test.Hspec               ( hspec
                                , Spec
                                , describe
                                )

import CLIArguments.LexerSpec   ( spec )
import CLIArguments.ParserSpec  ( spec )

main :: IO ()
main = hspec Main.spec

spec :: Spec
spec = do
    describe "CLI Arguments Lexer"      CLIArguments.LexerSpec.spec
    describe "CLI Arguments ParserSpec" CLIArguments.ParserSpec.spec
