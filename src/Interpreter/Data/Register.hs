--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Register
--

module Interpreter.Data.Register where

import Data.Map ( Map, fromList )
import qualified Data.Map as Map

import Interpreter.Parser     ( Tree(..) )
import Control.Exception      ( throw )
import Interpreter.Error      ( Error( UnknownName ) )

newtype RegisterId = RegisterId String
  deriving (Ord, Eq, Show)

newtype Register = Register (Map RegisterId EvaluatedValue)
  deriving (Show)

data EvaluatedValue = ValueNumber Double
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

regLookupMaybe :: Register -> RegisterId -> Maybe EvaluatedValue
regLookupMaybe (Register reg) id = Map.lookup id reg

regLookup :: Register -> RegisterId -> EvaluatedValue
regLookup (Register reg) id = regLookup' id $ Map.lookup id reg

regLookup' :: RegisterId -> Maybe EvaluatedValue -> EvaluatedValue
regLookup' _               (Just value)  = value
regLookup' (RegisterId id) Nothing       = throw $ UnknownName id

showValueList :: EvaluatedValue -> String
showValueList list = '(' : showValueList' False list ++ ")"

showValueList' :: Bool -> EvaluatedValue -> String
showValueList' _      (List (left@(List _), right@(List _)))  =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' _      (List (left@(List _), right))           =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' _      (List (left,          right@(List _)))  =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' False  (List (left,          ValueNil))        =       showValueList' True left
showValueList' False  (List (left,          right))           =       showValueList' True left ++ " . " ++ showValueList' False right
showValueList' True   (List (left,          ValueNil))        = '(' : showValueList' True left ++ ")"
showValueList' True   (List (left,          right))           = '(' : showValueList' True left ++ " . " ++ showValueList' False right ++ ")"
showValueList' _      other                                   = show other
