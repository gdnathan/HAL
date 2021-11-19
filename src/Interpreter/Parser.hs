--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Parser
--

module Interpreter.Parser ( buildExpressionsTrees
                          , RegisterId (..)
                          , ProcedureArg (..)
                          , NodeFirstElement (..)
                          , Tree (..)
                          ) where

-- import Interpreter.Precompilation  ( Token(..)
--                           -- , BuiltIns(..)
--                           )
-- import qualified Interpreter.Precompilation as Precompilation
import Interpreter.Lexer  ( Token(..)
                          -- , BuiltIns(..)
                          )
import qualified Interpreter.Lexer as Lexer
import GHC.Exception      ( throw )
import Interpreter.Error  ( Error(ParsingError) )

newtype RegisterId = RegisterId String
  deriving (Ord, Eq, Show)

data ProcedureArg = Number Float
                  | Symbol String
                  -- | Word   String
                  | UnCreatedList [Tree]
  deriving (Show)
                  --  Nil

data NodeFirstElement = Id RegisterId
                      | IdToGenerate Tree
  deriving (Show)

data Tree = Empty
          | Node    NodeFirstElement [Tree]
          | Leaf    ProcedureArg
  deriving (Show)
          --  Quoted  Tree

type UnderConstructionTree = ([Token], Tree)

buildExpressionsTrees :: [Token] -> [Tree]
buildExpressionsTrees = loopBuilding . launchParse

loopBuilding :: UnderConstructionTree -> [Tree]
loopBuilding ([],     tree) = [tree]
loopBuilding (tokens, tree) = tree : loopBuilding (launchParse tokens)

launchParse :: [Token] -> UnderConstructionTree
launchParse tokens = parseExpression (tokens, Empty)

parseExpression :: UnderConstructionTree -> UnderConstructionTree
parseExpression (ParenthesisOpen            : xs, _   ) = parseExpression' xs
parseExpression (Quote : ParenthesisOpen    : xs, _   ) = let (xss, res) = getArgs xs in (xss, Leaf $ UnCreatedList res)
parseExpression (Quote                      : xs, _   ) = wrapArgs (Id (RegisterId "quote")) $ getArg xs
parseExpression (ParenthesisClose           : xs, tree) = (xs, tree)
parseExpression (Lexer.Symbol str  : xs, _   ) = (xs, Leaf $ Interpreter.Parser.Symbol str)
parseExpression (Lexer.Number n    : xs, _   ) = (xs, Leaf $ Interpreter.Parser.Number n)
parseExpression _                                       = throw $ ParsingError "Invalid expression 1."

parseExpression' :: [Token] -> UnderConstructionTree
-- parseExpression' (Lexer.Word   name : xs) = wrapArgs (Id (RegisterId name))            $ getArgs xs
parseExpression' (Lexer.Symbol name : xs) = wrapArgs (Id (RegisterId name))            $ getArgs xs
parseExpression' tokens@(ParenthesisOpen     : _ ) = let (xss, funcId) = launchParse tokens in wrapArgs (IdToGenerate funcId) $ getArgs xss
parseExpression' _                                 = throw $ ParsingError "Invalid expression 2."
-- parseExpression' (BuiltIn builtIn             : xs) = wrapArgs (convertBuiltIns builtIn) $ getArgs xs
-- parseExpression' (TTrue                       : xs) = wrapArgs FTrue                     $ getArgs xs
-- parseExpression' (TFalse                      : xs) = wrapArgs FFalse                    $ getArgs xs

wrapArgs :: NodeFirstElement -> ([Token], [Tree]) -> UnderConstructionTree
wrapArgs funcId (tokens, args) = (tokens, Node funcId args)

getArg :: [Token] -> ([Token], [Tree])
getArg tokens = let (xs, arg) = launchParse xs in (xs, [arg])

getArgs :: [Token] -> ([Token], [Tree])
getArgs tokens = getArgs' [] $ launchParse tokens

getArgs' :: [Tree] -> UnderConstructionTree -> ([Token], [Tree])
getArgs' res (xs, Empty) = (xs, res)
getArgs' _   (xs, arg)   = let (xss, trees) = getArgs xs in (xss, arg : trees)

-- convertBuiltIns :: Interpreter.Lexer.BuiltIns -> Interpreter.Parser.FuncId
-- convertBuiltIns Interpreter.Lexer.Cons            = Interpreter.Parser.Cons
-- convertBuiltIns Interpreter.Lexer.Car             = Interpreter.Parser.Car
-- convertBuiltIns Interpreter.Lexer.Cdr             = Interpreter.Parser.Cdr
-- convertBuiltIns Interpreter.Lexer.IsEq            = Interpreter.Parser.IsEq
-- convertBuiltIns Interpreter.Lexer.IsAtom          = Interpreter.Parser.IsAtom
-- convertBuiltIns Interpreter.Lexer.Plus            = Interpreter.Parser.Plus
-- convertBuiltIns Interpreter.Lexer.Minus           = Interpreter.Parser.Minus
-- convertBuiltIns Interpreter.Lexer.Multiplication  = Interpreter.Parser.Multiplication
-- convertBuiltIns Interpreter.Lexer.Div             = Interpreter.Parser.Div
-- convertBuiltIns Interpreter.Lexer.Mod             = Interpreter.Parser.Mod
-- convertBuiltIns Interpreter.Lexer.Lt              = Interpreter.Parser.Lt
-- convertBuiltIns Interpreter.Lexer.Gt              = Interpreter.Parser.Gt
-- convertBuiltIns Interpreter.Lexer.Quote           = Interpreter.Parser.Quote
-- convertBuiltIns Interpreter.Lexer.Lambda          = Interpreter.Parser.Lambda
-- convertBuiltIns Interpreter.Lexer.Define          = Interpreter.Parser.Define
-- convertBuiltIns Interpreter.Lexer.Let             = Interpreter.Parser.Let
-- convertBuiltIns Interpreter.Lexer.Cond            = Interpreter.Parser.Cond
