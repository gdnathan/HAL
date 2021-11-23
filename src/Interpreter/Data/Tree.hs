--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Tree
--

module Interpreter.Data.Tree where

data ProcedureArg = Number Double
                  | Symbol String
                  | UnCreatedList [Tree]
  deriving (Show)

data Tree = Node    [Tree]
          | Leaf    ProcedureArg
  deriving (Show)