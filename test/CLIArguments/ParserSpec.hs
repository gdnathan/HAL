--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- CLIArguments LexerSpec
--

module CLIArguments.ParserSpec  ( spec ) where

import Test.Hspec               ( Spec
                                , it
                                )

import CLIArguments.Parser      ( parseArgs
                                , HalConfig(..)
                                , HalExecution(..)
                                )

newtype TestConfig = TestConfig HalConfig
instance Eq TestConfig where
    (TestConfig (HalConfig PrintHelp _))        == (TestConfig (HalConfig PrintHelp _))       = True
    (TestConfig (HalConfig Repl      files1))   == (TestConfig (HalConfig Repl      files2))  = files1 == files2
    (TestConfig (HalConfig Evaluate  files1))   == (TestConfig (HalConfig Evaluate  files2))  = files1 == files2
    _                                           == _                                          = False

spec :: Spec
spec = do
    it "Correct - No arguments given" $ do
        TestConfig (parseArgs [])
            == TestConfig (HalConfig Repl [])
    it "Correct - Only one file given" $ do
        TestConfig (parseArgs ["file1"])
            == TestConfig (HalConfig Evaluate ["file1"])
    it "Correct - Multiple files given" $ do
        TestConfig (parseArgs ["file1", "file2", "file3"])
            == TestConfig (HalConfig Evaluate ["file1", "file2", "file3"])
    it "Correct - Multiple files given REPL" $ do
        TestConfig (parseArgs ["-i", "file1", "file2", "file3"])
            == TestConfig (HalConfig Repl ["file1", "file2", "file3"])
    it "Correct - short help option given" $ do
        TestConfig (parseArgs ["-h"])
            == TestConfig (HalConfig PrintHelp [])
    it "Correct - long help option given" $ do
        TestConfig (parseArgs ["--help"])
            == TestConfig (HalConfig PrintHelp [])
    it "Correct - short help option given" $ do
        TestConfig (parseArgs ["-i"])
            == TestConfig (HalConfig Repl [])
    it "Correct - long help option given" $ do
        TestConfig (parseArgs ["--repl"])
            == TestConfig (HalConfig Repl [])
