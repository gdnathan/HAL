--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments LexerSpec
--

module CLIArguments.LexerSpec   ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                )

import CLIArguments.Lexer       ( Token(..)
                                , tokenizeArgs
                                )

newtype TestToken = TestToken Token
instance Eq TestToken where
    (TestToken Help)        == (TestToken Help)         = True
    (TestToken LaunchRepl)  == (TestToken LaunchRepl)   = True
    (TestToken (File left)) == (TestToken (File right)) = True
    _                       == _                        = False

spec :: Spec
spec = do
    it "Correct - No arguments given" $ do
        null (map TestToken (tokenizeArgs []))
    it "Correct - Only one file given" $ do
        map TestToken (tokenizeArgs ["file1"])
            == map TestToken [File "file1"]
    it "Correct - Multiple files given" $ do
        map TestToken (tokenizeArgs ["file1", "file2", "file3"])
            == map TestToken [File "file1", File "file2", File "file3"]
    it "Correct - Multiple files given REPL" $ do
        map TestToken (tokenizeArgs ["-i", "file1", "file2", "file3"])
            == map TestToken [LaunchRepl, File "file1", File "file2", File "file3"]
    it "Correct - short help option given" $ do
        map TestToken (tokenizeArgs ["-h"])
            == map TestToken [Help]
    it "Correct - long help option given" $ do
        map TestToken (tokenizeArgs ["--help"])
            == map TestToken [Help]
    it "Correct - short help option given" $ do
        map TestToken (tokenizeArgs ["-i"])
            == map TestToken [LaunchRepl]
    it "Correct - long help option given" $ do
        map TestToken (tokenizeArgs ["--repl"])
            == map TestToken [LaunchRepl]
