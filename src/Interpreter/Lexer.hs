--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter Lexer
--

module Interpreter.Lexer      ( tokenize ) where

import GHC.Unicode            ( isDigit )
import Control.Exception.Base ( throw )
import Text.Read              ( readMaybe )

import Interpreter.Error      ( Error(..) )
import Interpreter.Data.Token ( Token(..) )

data LexingToken =  LParenthesisOpen
                  | LParenthesisClose
                  | LQuote
                  | LSpace
                  | LWord   String

tokenize :: String -> [Token]
tokenize = convertLexingToken . foldr tokenize' []

tokenize' :: Char -> [LexingToken] -> [LexingToken]
tokenize' '('   t               = LParenthesisOpen  : t
tokenize' ')'   t               = LParenthesisClose : t
tokenize' '\''  t               = LQuote            : t
tokenize' ' '   t               = LSpace            : t
tokenize' '\t'  t               = LSpace            : t
tokenize' '\n'  t               = LSpace            : t
tokenize' x     (LWord a : ts)  = LWord (x : a)     : ts
tokenize' x     t               = LWord [x]         : t

convertLexingToken :: [LexingToken] -> [Token]
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
