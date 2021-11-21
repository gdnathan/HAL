--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- All
--

module Interpreter.Builtins.All ( initialRegister ) where

import Data.Map                         ( fromList )
import Interpreter.Data.Register        ( Register ( Register )
                                        , RegisterId ( RegisterId )
                                        , EvaluatedValue ( Procedure )
                                        )
import Interpreter.Builtins.List        ( cons
                                        , car
                                        , cdr
                                        )
import Interpreter.Builtins.Test        ( isEq
                                        , isAtom
                                        , cond
                                        )
import Interpreter.Builtins.Arithmetic  ( add
                                        , sub
                                        , multiplication
                                        , divProcedure
                                        , modProcedure
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
                                      , (RegisterId "+",      Procedure add)
                                      , (RegisterId "-",      Procedure sub)
                                      , (RegisterId "*",      Procedure multiplication)
                                      , (RegisterId "div",    Procedure divProcedure)
                                      , (RegisterId "mod",    Procedure modProcedure)
                                      , (RegisterId "<",      Procedure ltProcedure)
                                      , (RegisterId "<=",     Procedure lteProcedure)
                                      , (RegisterId ">",      Procedure gtProcedure)
                                      , (RegisterId ">=",     Procedure gteProcedure)
                                      , (RegisterId "quote",  Procedure quote)
                                      , (RegisterId "lambda", Procedure lambda)
                                      , (RegisterId "let",    Procedure letProcedure)
                                      , (RegisterId "cond",   Procedure cond)
                                      ]
