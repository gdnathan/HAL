--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- Interpreter Parser
--

module Interpreter.Parser           ( buildExpressionsTrees
                                    , Tree(..)
                                    , ProcedureArg(..)
                                    ) where

import GHC.Exception                ( throw )

import Interpreter.Error            ( Error( InvalidSyntax ) )
import Interpreter.Lexer as Lexer   ( Token(..)
                                    , NumbersType
                                    )

data ProcedureArg = Number NumbersType
                  | Symbol String
                  | UncreatedList [Tree]

data Tree = Node [Tree]
          | Leaf ProcedureArg

type UnderConstructionTree  = ([Token], Maybe Tree)
type ParsedArgs             = ([Token], [Tree])

buildExpressionsTrees :: [Token] -> [Tree]
buildExpressionsTrees = constructTreesAndLoop . launchExprParse

constructTreesAndLoop :: UnderConstructionTree -> [Tree]
constructTreesAndLoop ([],     Just tree)  = [tree]
constructTreesAndLoop (tokens, Just tree)  = tree : constructTreesAndLoop (launchExprParse tokens)
constructTreesAndLoop (_,      Nothing)    = throw $ InvalidSyntax "an expression can not be empty"

launchExprParse :: [Token] -> UnderConstructionTree
launchExprParse tokens = parseExpr (tokens, Nothing)

parseExpr :: UnderConstructionTree -> UnderConstructionTree
parseExpr (ParenthesisOpen            : xs, _   ) = wrapArgs $ getArgs xs
parseExpr (ParenthesisClose           : xs, tree) = (xs, tree)
parseExpr (Lexer.Symbol str           : xs, _   ) = (xs, Just $ Leaf $ Interpreter.Parser.Symbol str)
parseExpr (Lexer.Number n             : xs, _   ) = (xs, Just $ Leaf $ Interpreter.Parser.Number n)
parseExpr (Quote : ParenthesisOpen    : xs, _   ) = wrapUncreatedList $ getArgs xs
parseExpr (Quote                      : xs, _   ) = wrapArgsWithHeader (getArg  xs) $ Leaf $ Interpreter.Parser.Symbol "quote"
parseExpr ([], tree)                              = ([], tree)

wrapArgs :: ParsedArgs -> UnderConstructionTree
wrapArgs (tokens, args) = (tokens, Just $ Node args)

wrapUncreatedList :: ParsedArgs -> UnderConstructionTree
wrapUncreatedList (tokens, args) = (tokens, Just $ Leaf $ UncreatedList args)

wrapArgsWithHeader :: ParsedArgs -> Tree -> UnderConstructionTree
wrapArgsWithHeader (tokens, args) funcId = (tokens, Just $ Node (funcId : args))

getArgs :: [Token] -> ParsedArgs
getArgs = getArgs' . launchExprParse

getArgs' :: UnderConstructionTree -> ParsedArgs
getArgs' (xs, Nothing)  = (xs, [])
getArgs' (xs, Just arg) = getArgs'' arg $ getArgs xs

getArgs'' :: Tree -> ParsedArgs -> ParsedArgs
getArgs'' arg (tokens, trees) = (tokens, arg : trees)

getArg :: [Token] -> ParsedArgs
getArg = getArg' . launchExprParse

getArg' :: UnderConstructionTree -> ParsedArgs
getArg' (xs, Just arg) = (xs, [arg])
getArg' (xs, Nothing)  = (xs, [])
