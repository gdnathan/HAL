module ArgumentLexer ( Token(..)
                     , tokenizeArgs
                     ) where

data Token = HELP
           | LAUNCH_REPL
           | FILE String

tokenizeArgs :: [String] -> [Token]
tokenizeArgs []            = []
tokenizeArgs ("-h":xs)     = HELP : tokenizeArgs xs
tokenizeArgs ("--help":xs) = HELP : tokenizeArgs xs
tokenizeArgs ("-i":xs)     = LAUNCH_REPL : tokenizeArgs xs
tokenizeArgs (x:xs)        = FILE x : tokenizeArgs xs
