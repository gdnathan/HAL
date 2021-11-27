--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
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
      show (ValueNumber value)  = show $ round value
      show (ValueName   value)  = value
      show NoValue              = ""
      show ValueNil             = "()"
      show ValueFalse           = "#f"
      show ValueTrue            = "#t"
      show list@(List _)        = showValueList list
      show (Procedure _)        = "#<procedure>"
instance Eq EvaluatedValue where
      (==) (ValueNumber x) (ValueNumber y)  = x == y
      (==) (ValueName   x) (ValueName   y)  = x == y
      (==)  ValueNil        ValueNil        = True
      (==)  _               _               = False

regInsert :: Register -> RegisterId -> EvaluatedValue -> Register
regInsert (Register reg) id value = Register $ Map.insert id value reg

regInsertRange :: Register -> [RegisterId] -> [EvaluatedValue] -> Register
regInsertRange reg  []        _                 = reg
regInsertRange reg  _         []                = reg
regInsertRange reg (id : ids) (value : values)  = regInsertRange (regInsert reg id value) ids values

regInsertRange2 :: Register -> ([RegisterId], [EvaluatedValue]) -> Register
regInsertRange2 reg (ids, values) = regInsertRange reg ids values

regLookup :: Register -> RegisterId -> EvaluatedValue
regLookup (Register reg) id = regLookup' id $ Map.lookup id reg

regLookup' :: RegisterId -> Maybe EvaluatedValue -> EvaluatedValue
regLookup' _               (Just value)  = value
regLookup' (RegisterId id) Nothing       = throw $ UnknownName id

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
