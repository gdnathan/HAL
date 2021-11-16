--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Lexer
--

module Interpreter.Lexer  ( tokenize
                          , Token(..)
                          , BuiltIns(..)
                          ) where

import GHC.Unicode                ( isDigit )

import Control.Exception.Base (throw)
import Text.Read (readMaybe)
import Interpreter.Error (Error(NameStartWithNumber, ParsingError))

data BuiltIns = Cons
              | Car
              | Cdr
              | IsEq
              | IsAtom
              | Plus
              | Minus
              | Multiplication
              | Div
              | Mod
              | Lt
              | Gt
              | Quote
              | Lambda
              | Define
              | Let
              | Cond
  deriving (Show)

data Token =  ParenthesisOpen
            | ParenthesisClose
            | TTrue
            | TFalse
            | Nil
            | BuiltIn BuiltIns
            | Number Float
            | Word String
  deriving (Show)

data LexingToken =  LParenthesisOpen
                  | LParenthesisClose
                  | LQuote
                  | LTrue
                  | LFalse
                  | Space
                  | LWord   String

tokenize :: String -> [Token]
tokenize = convertLexingToken . foldr tokenize' []

tokenize' :: Char -> [LexingToken] -> [LexingToken]
tokenize' '('   t               = LParenthesisOpen  : t
tokenize' ')'   t               = LParenthesisClose : t
tokenize' '\''  t               = LQuote            : t
tokenize' ' '   t               = Space             : t
tokenize' '\t'  t               = Space             : t
tokenize' '\n'  t               = Space             : t
tokenize' x     (LWord a : ts)  = LWord (x : a)     : ts
tokenize' x     t               = LWord [x]         : t

convertLexingToken :: [LexingToken] -> [Token]
convertLexingToken []                       = []
convertLexingToken (Space             : xs) = convertLexingToken xs
convertLexingToken (LParenthesisOpen  : xs) = ParenthesisOpen   : convertLexingToken xs
convertLexingToken (LParenthesisClose : xs) = ParenthesisClose  : convertLexingToken xs
convertLexingToken (LQuote            : xs) = BuiltIn Quote     : convertLexingToken xs
convertLexingToken (LTrue             : xs) = TTrue             : convertLexingToken xs
convertLexingToken (LFalse            : xs) = TFalse            : convertLexingToken xs
convertLexingToken (LWord str         : xs) = identifyWord str  : convertLexingToken xs

identifyWord :: String -> Token
identifyWord "nil"    = Nil
identifyWord "#t"     = TTrue
identifyWord "#f"     = TFalse
identifyWord "cons"   = BuiltIn Cons
identifyWord "car"    = BuiltIn Car
identifyWord "cdr"    = BuiltIn Cdr
identifyWord "eq?"    = BuiltIn IsEq
identifyWord "atom?"  = BuiltIn IsAtom
identifyWord "+"      = BuiltIn Plus
identifyWord "-"      = BuiltIn Minus
identifyWord "*"      = BuiltIn Multiplication
identifyWord "div"    = BuiltIn Div
identifyWord "mod"    = BuiltIn Mod
identifyWord "<"      = BuiltIn Lt
identifyWord ">"      = BuiltIn Gt
identifyWord "quote"  = BuiltIn Quote
identifyWord "'"      = BuiltIn Quote
identifyWord "lambda" = BuiltIn Lambda
identifyWord "define" = BuiltIn Define
identifyWord "let"    = BuiltIn Let
identifyWord "cond"   = BuiltIn Cond
identifyWord str@(x:_)
          | isDigit x = Number $ readAndCheck str
          | otherwise = Word str
identifyWord _        = throw $ ParsingError "This can not happen, but ide is bugged"

readAndCheck :: String -> Float
readAndCheck name = readAndCheck' name $ readMaybe name

readAndCheck' :: String -> Maybe Float -> Float
readAndCheck' _     (Just x)  = x
readAndCheck' name  Nothing   = throw $ NameStartWithNumber name
