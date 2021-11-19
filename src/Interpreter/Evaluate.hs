--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Evaluate
--

module Interpreter.Evaluate where

import Data.Map ( Map, fromList )
import qualified Data.Map as Map
import Control.Exception.Base (throw, evaluate)
import Interpreter.Error (Error(InternalError, ParsingError, UnknownName, InvalidSyntax, InvalidNumberOfArguments, BuiltInError, NotAProcedure), BuiltInError (ArgumentIsNotAPair))
import Interpreter.Parser ( buildExpressionsTrees
                          , RegisterId (..)
                          , ProcedureArg (..)
                          , NodeFirstElement (..)
                          , Tree (..)
                          )
import Interpreter.Lexer (tokenize)
import Control.Arrow (ArrowChoice(left, right))
-- import Interpreter.Builtins (initialRegister)
-- import Interpreter.Parser (Tree(..), ProcedureArg(..))

-- newtype RegisterId = RegisterId String
--   deriving (Ord, Eq, Show)

-- data ProcedureArg = Number Float
--                   | Symbol String
--                   | Word   String
--                   | UnCreatedList [Tree]
--                   | Nil
--   deriving (Show)

-- data NodeFirstElement = Id RegisterId
--                       | IdToGenerate Tree
--   deriving (Show)

-- data Tree = Empty
--           | Node    NodeFirstElement [Tree]
--           | Leaf    ProcedureArg
--   deriving (Show)
          --  Quoted  Tree
-- (cons 1 (cons 2 3))
data EvaluatedValue = ValueNumber Float
                    | ValueName   String
                    | NoValue
                    | ValueNil
                    | ValueFalse
                    | ValueTrue
                    | List        (EvaluatedValue, EvaluatedValue)
                    | Procedure   (Register -> [Tree] -> EvaluatedValue)
instance Show EvaluatedValue where
      show (ValueNumber value)  = show value
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

newtype Register = Register (Map RegisterId EvaluatedValue)
  deriving (Show)

newtype EvaluatingContext = Context (Register, Tree)
  deriving (Show)
newtype EvaluationResult  = Result  (Register, EvaluatedValue)
  deriving (Show)

evaluateTree :: EvaluatingContext -> EvaluationResult
evaluateTree (Context (reg, Node (Id (RegisterId "define")) args))  = defineValue reg args
evaluateTree context@(Context (reg, _))                             = Result (reg, evaluateValue context)

defineValue :: Register -> [Tree] -> EvaluationResult
defineValue reg  [Node (Id id@(RegisterId name)) argsName , body] = Result (regInsert reg id $ createProcedure argsName body, ValueName name)
defineValue reg  [Leaf (Symbol name)                      , body] = Result (regInsert reg (RegisterId name) $ evaluateValue (Context (reg, body)), ValueName name)
defineValue _               _                                     = throw $ InvalidSyntax "Invalid define definition"

-- defineValueOrProcedure :: Register -> String -> EvaluatedValue -> EvaluationResult
-- defineValueOrProcedure reg (Proc)

createProcedureFromNode :: Tree -> Tree -> EvaluatedValue
createProcedureFromNode (Node (Id (RegisterId firstName)) otherArgs)  body = createProcedure (Leaf (Symbol firstName) : otherArgs) body
createProcedureFromNode _                                             _    = throw $ InternalError "Only nodes are accepted"

createProcedure :: [Tree] -> Tree -> EvaluatedValue
createProcedure argsName body = Procedure $ \reg args -> evaluateValue $ Context (addArgsToRegister (argsToIdsList argsName) (evaluateArgs reg args) reg, body)

argsToIdsList :: [Tree] -> [RegisterId]
argsToIdsList []                          = []
argsToIdsList ((Leaf (Symbol name)) : xs) = RegisterId name : argsToIdsList xs
argsToIdsList _                           = throw $ InvalidSyntax "argument name must be a string"

-- if there is already a var named same as arg name, should throw
addArgsToRegister :: [RegisterId] -> [EvaluatedValue] -> Register -> Register
addArgsToRegister []        []        reg             = reg
addArgsToRegister _         []        _               = throw InvalidNumberOfArguments
addArgsToRegister []        _         _               = throw InvalidNumberOfArguments
addArgsToRegister (x : xs)  (y : ys)  (Register reg)  = addArgsToRegister xs ys $ Register $ Map.insert x y reg


evaluateValue :: EvaluatingContext -> EvaluatedValue
evaluateValue (Context (_  , Empty   ))                         = ValueNil
evaluateValue (Context (reg, Node    (Id            id) args))  = callProcedure reg (regLookup reg id) args
evaluateValue (Context (reg, Node    (IdToGenerate  id) args))  = callProcedure reg (evaluateValue (Context (reg, id))) args
evaluateValue (Context (reg, Leaf    arg))                      = convertProcedureArg reg arg
-- evaluateValue (Context (reg, Quoted  tree))                     = Pure tree

convertProcedureArg :: Register -> ProcedureArg -> EvaluatedValue
convertProcedureArg _   (Number n)            = ValueNumber n
convertProcedureArg _   (Symbol   "#t")       = ValueTrue
convertProcedureArg _   (Symbol   "#f")       = ValueFalse
convertProcedureArg reg (Symbol   name)       = regLookup reg (RegisterId name)
-- convertProcedureArg _   (Word     str)        = ValueName str
-- convertProcedureArg _    Nil                  = ValueNil
convertProcedureArg reg (UnCreatedList list)  = createList reg list

createList :: Register -> [Tree] -> EvaluatedValue
createList _    []          = ValueNil
createList reg  (left : xs) = List (evaluateValue (Context (reg, left)), createList reg xs)

callProcedure :: Register -> EvaluatedValue -> [Tree] -> EvaluatedValue
callProcedure reg (Procedure    body) args  = body reg args
callProcedure reg (ValueName    name) args  = callProcedure reg (regLookup reg (RegisterId name)) args
-- callProcedure reg (Pure         tree) args  = callProcedure reg (evaluateValue (Context (reg, tree))) args
callProcedure _   _                   _     = throw NotAProcedure -- "attempt to apply non-procedure"

evaluateArgs :: Register -> [Tree] -> [EvaluatedValue]
evaluateArgs _    []        = []
evaluateArgs reg  (x : xs)  = evaluateValue (Context (reg, x)) : evaluateArgs reg xs

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
showValueList list = '(' : showValueList' True list ++ ")"

showValueList' :: Bool -> EvaluatedValue -> String
showValueList' _      (List (left@(List _), right@(List _)))  =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' _      (List (left@(List _), right))           =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' _      (List (left,          right@(List _)))  =       showValueList' True left ++  " "  ++ showValueList' False right
showValueList' False  (List (left,          right))           =       showValueList' True left ++ " . " ++ showValueList' False right
showValueList' True   (List (left,          right))           = '(' : showValueList' True left ++ " . " ++ showValueList' False right ++ ")"
showValueList' _      other                                   = show other
