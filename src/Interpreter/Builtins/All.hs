--
-- EPITECH PROJECT, 2021
-- HAL
-- File description:
-- All
--

module Interpreter.Builtins.All         ( initialRegister ) where

import Data.Map                         ( fromList )
import Interpreter.Register             ( Register( Register )
                                        , RegisterId( RegisterId )
                                        , EvaluatedValue( Procedure )
                                        )
import Interpreter.Builtins.List        ( cons
                                        , car
                                        , cdr
                                        )
import Interpreter.Builtins.Test        ( isEq
                                        , isAtom
                                        , cond
                                        )
import Interpreter.Builtins.Arithmetic  ( addition
                                        , subtraction
                                        , multiplication
                                        , division
                                        , modulo
                                        )
import Interpreter.Builtins.Ord         ( ltProcedure
                                        , lteProcedure
                                        , gtProcedure
                                        , gteProcedure
                                        )
import Interpreter.Builtins.Quote       ( quote )
import Interpreter.Builtins.Lambda      ( lambda
                                        , letProcedure
                                        )

initialRegister :: Register
initialRegister = Register $ fromList [ (RegisterId "cons",   Procedure cons)
                                      , (RegisterId "car",    Procedure car)
                                      , (RegisterId "cdr",    Procedure cdr)
                                      , (RegisterId "eq?",    Procedure isEq)
                                      , (RegisterId "atom?",  Procedure isAtom)
                                      , (RegisterId "+",      Procedure addition)
                                      , (RegisterId "-",      Procedure subtraction)
                                      , (RegisterId "*",      Procedure multiplication)
                                      , (RegisterId "div",    Procedure division)
                                      , (RegisterId "mod",    Procedure modulo)
                                      , (RegisterId "<",      Procedure ltProcedure)
                                      , (RegisterId "<=",     Procedure lteProcedure)
                                      , (RegisterId ">",      Procedure gtProcedure)
                                      , (RegisterId ">=",     Procedure gteProcedure)
                                      , (RegisterId "quote",  Procedure quote)
                                      , (RegisterId "lambda", Procedure lambda)
                                      , (RegisterId "let",    Procedure letProcedure)
                                      , (RegisterId "cond",   Procedure cond)
                                      ]
