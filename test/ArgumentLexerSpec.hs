module ArgumentLexerSpec ( spec ) where

import Test.Hspec        ( Spec
                         , it )

import ArgumentLexer     ( Token(..)
                         , tokenizeArgs )

newtype TestToken = TestToken Token
instance Eq TestToken where
    (TestToken HELP)        == (TestToken HELP)         = True
    (TestToken LAUNCH_REPL) == (TestToken LAUNCH_REPL)  = True
    (TestToken (FILE left)) == (TestToken (FILE right)) = True
    _                       == _                        = False

spec :: Spec
spec = do
    it "Argument Lexer: No arguments given" $ do
        null (map TestToken (tokenizeArgs []))
    it "Argument Lexer: Only one file given" $ do
        map TestToken (tokenizeArgs ["./test/tests_files/sort.scm"]) == map TestToken [FILE "./test/tests_files/sort.scm"]
    it "Argument Lexer: Multiple files given" $ do
        map TestToken (tokenizeArgs ["./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]) == map TestToken [FILE "./test/tests_files/fact.lisp", FILE "./test/tests_files/fib.lisp", FILE "./test/tests_files/sort.scm"]
    it "Argument Lexer: Multiple files given REPL" $ do
        map TestToken (tokenizeArgs ["-i", "./test/tests_files/fact.lisp", "./test/tests_files/fib.lisp", "./test/tests_files/sort.scm"]) == map TestToken [LAUNCH_REPL, FILE "./test/tests_files/fact.lisp", FILE "./test/tests_files/fib.lisp", FILE "./test/tests_files/sort.scm"]
