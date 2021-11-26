--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter Lexer
--

module Interpreter.Lexer        ( tokenize
                                , Token(..)
                                ) where

import GHC.Unicode              ( isDigit )
import Control.Exception.Base   ( throw )
import Text.Read                ( readMaybe )

import Interpreter.Error        ( Error(..) )

data Token  = ParenthesisOpen
            | ParenthesisClose
            | Quote
            | Number Double
            | Symbol String

data InLexingToken =  LParenthesisOpen
                    | LParenthesisClose
                    | LQuote
                    | LSpace
                    | LWord String

tokenize :: String -> [Token]
tokenize = convertLexingToken . foldr tokenize' []

tokenize' :: Char -> [InLexingToken] -> [InLexingToken]
tokenize' '('   token                 = LParenthesisOpen  : token
tokenize' ')'   token                 = LParenthesisClose : token
tokenize' '\''  token                 = LQuote            : token
tokenize' ' '   token                 = LSpace            : token
tokenize' '\t'  token                 = LSpace            : token
tokenize' '\n'  token                 = LSpace            : token
tokenize' x     (LWord word : tokens) = LWord (x : word)  : tokens
tokenize' x     token                 = LWord [x]         : token

convertLexingToken :: [InLexingToken] -> [Token]
convertLexingToken []                       = []
convertLexingToken (LSpace            : xs) = convertLexingToken xs
convertLexingToken (LParenthesisOpen  : xs) = ParenthesisOpen         : convertLexingToken xs
convertLexingToken (LParenthesisClose : xs) = ParenthesisClose        : convertLexingToken xs
convertLexingToken (LQuote            : xs) = Quote                   : convertLexingToken xs
convertLexingToken (LWord str         : xs) = wordToWordOrNumber str  : convertLexingToken xs

wordToWordOrNumber :: String -> Token
wordToWordOrNumber str = wordToWordOrNumber' str $ readMaybe str

wordToWordOrNumber' :: String -> Maybe Double -> Token
wordToWordOrNumber' name Nothing  = Symbol name
wordToWordOrNumber' _    (Just n) = Number n
