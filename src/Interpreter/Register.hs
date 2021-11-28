--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Register
--

module Interpreter.Register   ( regInsert
                              , regInsertRange
                              , regInsertRange2
                              , regLookup
                              , RegisterId(..)
                              , Register(..)
                              , EvaluatedValue(..)
                              ) where

import Data.Map as Map        ( Map
                              , lookup
                              , insert
                              )
import Control.Exception      ( throw )

import Interpreter.Error      ( Error( UnknownName ) )
import Interpreter.Lexer      ( NumbersType )
import Interpreter.Parser     ( Tree )

newtype RegisterId = RegisterId String
  deriving (Eq, Ord)

newtype Register = Register (Map RegisterId EvaluatedValue)

data EvaluatedValue = ValueNumber NumbersType
                    | ValueName   String
                    | NoValue
                    | ValueNil
                    | ValueFalse
                    | ValueTrue
                    | List        (EvaluatedValue, EvaluatedValue)
                    | Procedure   (Register -> [Tree] -> EvaluatedValue)
instance Show EvaluatedValue where
      show (ValueNumber value)  = show (round value :: Integer)
      show (ValueName   value)  = value
      show NoValue              = ""
      show ValueNil             = "()"
      show ValueFalse           = "#f"
      show ValueTrue            = "#t"
      show list@(List _)        = showValueList list
      show (Procedure _)        = "#<procedure>"

regInsert :: Register -> RegisterId -> EvaluatedValue -> Register
regInsert (Register reg) rId value = Register $ Map.insert rId value reg

regInsertRange :: Register -> [RegisterId] -> [EvaluatedValue] -> Register
regInsertRange reg  []        _                 = reg
regInsertRange reg  _         []                = reg
regInsertRange reg (rId : ids) (value : values)  = regInsertRange (regInsert reg rId value) ids values

regInsertRange2 :: Register -> ([RegisterId], [EvaluatedValue]) -> Register
regInsertRange2 reg (ids, values) = regInsertRange reg ids values

regLookup :: Register -> RegisterId -> EvaluatedValue
regLookup (Register reg) rId = regLookup' rId $ Map.lookup rId reg

regLookup' :: RegisterId -> Maybe EvaluatedValue -> EvaluatedValue
regLookup' _               (Just value)  = value
regLookup' (RegisterId rId) Nothing       = throw $ UnknownName rId

showValueList :: EvaluatedValue -> String
showValueList = showValueList' True

showValueList' :: Bool -> EvaluatedValue -> String
showValueList' _ (List (ValueName "quote",  List (val, ValueNil)))  = '\'' : show val
showValueList' p (List (left,               right@(List _)))        = wrapWithParenthesis p $ showValueList' True left ++  ' '  :  showValueList' False right
showValueList' p (List (left,               ValueNil))              = wrapWithParenthesis p $ showValueList' True left
showValueList' p (List (left,               right))                 = wrapWithParenthesis p $ showValueList' True left ++ " . " ++ showValueList' False right
showValueList' _ other                                              = show other

wrapWithParenthesis :: Bool -> String -> String
wrapWithParenthesis True  str = '(' : str ++ ")"
wrapWithParenthesis False str = str
