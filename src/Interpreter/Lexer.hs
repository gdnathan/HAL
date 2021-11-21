--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Lexer
--

module Interpreter.Lexer  ( tokenize ) where

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
wordToWordOrNumber str@(x : _)
                    | isDigit x = Number $ readAndCheck str
                    | otherwise = Symbol str
wordToWordOrNumber _            = throw $ InternalError "A word should never be empty"

readAndCheck :: String -> Float
readAndCheck name = readAndCheck' name $ readMaybe name

readAndCheck' :: String -> Maybe Float -> Float
readAndCheck' _     (Just x)  = x
readAndCheck' name  Nothing   = throw $ NameStartWithNumber name
