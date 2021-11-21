--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Parser
--

module Interpreter.Parser       ( buildExpressionsTrees ) where

import GHC.Exception            ( throw )

import Interpreter.Error        ( Error( InvalidSyntax ) )
import Interpreter.Data.Token   ( Token(..) )
import qualified Interpreter.Data.Token as Token
import Interpreter.Data.Tree    ( Tree(..), ProcedureArg(..) )
import qualified Interpreter.Data.Tree as Tree

type UnderConstructionTree  = ([Token], Maybe Tree)
type ParsedArgs             = ([Token], [Tree])

buildExpressionsTrees :: [Token] -> [Tree]
buildExpressionsTrees = loopBuilding . launchExprParse

loopBuilding :: UnderConstructionTree -> [Tree]
loopBuilding ([],     Just tree)  = [tree]
loopBuilding (tokens, Just tree)  = tree : loopBuilding (launchExprParse tokens)
loopBuilding (_,      Nothing)    = throw $ InvalidSyntax "an expression can not be empty"

launchExprParse :: [Token] -> UnderConstructionTree
launchExprParse tokens = parseExpr (tokens, Nothing)

parseExpr :: UnderConstructionTree -> UnderConstructionTree
parseExpr (ParenthesisOpen            : xs, _   ) = wrapArgs $ getArgs xs
parseExpr (ParenthesisClose           : xs, tree) = (xs, tree)
parseExpr (Token.Symbol str           : xs, _   ) = (xs, Just $ Leaf $ Tree.Symbol str)
parseExpr (Token.Number n             : xs, _   ) = (xs, Just $ Leaf $ Tree.Number n)
parseExpr (Quote : ParenthesisOpen    : xs, _   ) = let (xss, res) = getArgs xs in (xss, Just $ Leaf $ UnCreatedList res)
parseExpr (Quote                      : xs, _   ) = wrapArgsWithHeader (Leaf (Tree.Symbol "quote")) $ getArg xs
parseExpr ([], tree)                              = ([], tree)

wrapArgsWithHeader :: Tree -> ParsedArgs -> UnderConstructionTree
wrapArgsWithHeader funcId (tokens, args) = (tokens, Just $ Node (funcId : args))

wrapArgs :: ParsedArgs -> UnderConstructionTree
wrapArgs (tokens, args) = (tokens, Just $ Node args)

getArg :: [Token] -> ParsedArgs
getArg tokens = getArg' $ launchExprParse tokens

getArg' :: UnderConstructionTree -> ParsedArgs
getArg' (xs, Just arg) = (xs, [arg])
getArg' (xs, Nothing ) = (xs, [])

getArgs :: [Token] -> ParsedArgs
getArgs tokens = getArgs' [] $ launchExprParse tokens

getArgs' :: [Tree] -> UnderConstructionTree -> ParsedArgs
getArgs' res (xs, Nothing) = (xs, res)
getArgs' _   (xs, Just arg)   = let (xss, trees) = getArgs xs in (xss, arg : trees)
