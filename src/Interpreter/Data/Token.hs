--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Token
--

module Interpreter.Data.Token where

data Token  = ParenthesisOpen
            | ParenthesisClose
            | Quote
            | Number Float
            | Symbol String
  deriving (Show)