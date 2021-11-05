module ArgumentParserSpec ( spec ) where

import Test.Hspec         ( Spec
                          , it )

import ArgumentParser     ( HalEvaluationMode(..)
                          , HalArgs(..)
                          , parseArgs )

newtype TestHalEvaluationMode = TestHalEvaluationMode HalEvaluationMode
instance Eq TestHalEvaluationMode where
    (TestHalEvaluationMode EVALUATE) == (TestHalEvaluationMode EVALUATE) = True
    (TestHalEvaluationMode REPL)     == (TestHalEvaluationMode REPL)     = True
    _                                == _                                = False

newtype TestHalArgs = TestHalArgs HalArgs
instance Eq TestHalArgs where
    (TestHalArgs (HalArgs lem lf)) == (TestHalArgs (HalArgs rem rf)) = TestHalEvaluationMode lem == TestHalEvaluationMode rem && lf == rf

spec :: Spec
spec = do
    it "Argument Parser: No arguments given" $ do
        TestHalArgs (parseArgs []) == TestHalArgs (HalArgs REPL Nothing)
    it "Argument Parser: Only one file given" $ do
        TestHalArgs (parseArgs ["./test/tests_files/sort.scm"]) == TestHalArgs (HalArgs EVALUATE (Just ["./test/tests_files/sort.scm"]))
    it "Argument Parser: Multiple files given" $ do
        TestHalArgs (parseArgs ["./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]) == TestHalArgs (HalArgs EVALUATE (Just ["./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]))
    it "Argument Parser: Multiple files given REPL" $ do
        TestHalArgs (parseArgs ["-i", "./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]) == TestHalArgs (HalArgs REPL (Just ["./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]))
