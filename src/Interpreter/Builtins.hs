--
-- EPITECH PROJECT, 2021
-- B-FUN-501-BDX-5-1-HAL-guillaume.bogard-coquard
-- File description:
-- Builtins
--

module Interpreter.Builtins where
import Interpreter.Evaluate (Register(..), EvaluatedValue(..), evaluateValue, EvaluatingContext (..), createList, createProcedureFromNode, EvaluationResult (Result), regInsertRange2)
import Interpreter.Parser (RegisterId(..), NodeFirstElement (..), Tree (..), ProcedureArg (..))
import Interpreter.Error (Error(..), BuiltInError (..))
import Control.Exception (throw)
import Data.Map (fromList)
import Data.Fixed (mod')

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

--- Builtin ---
cons :: Register -> [Tree] -> EvaluatedValue
cons reg [left, right]  = List (evaluateValue $ Context (reg, left), evaluateValue $ Context (reg, right))
cons _   _              = throw InvalidNumberOfArguments

--- Builtin ---
car :: Register -> [Tree] -> EvaluatedValue
car reg [list]  = car' $ evaluateValue $ Context (reg, list)
car _   _       = throw InvalidNumberOfArguments

car' :: EvaluatedValue -> EvaluatedValue
car' (List (left, _)) = left
car' _                = throw $ BuiltInError "cdr" ArgumentIsNotAPair

--- Builtin ---
cdr :: Register -> [Tree] -> EvaluatedValue
cdr reg [list]  = cdr' $ evaluateValue $ Context (reg, list)
cdr _   _       = throw InvalidNumberOfArguments

cdr' :: EvaluatedValue -> EvaluatedValue
cdr' (List (_, right))  = right
cdr' _                  = throw $ BuiltInError "cdr" ArgumentIsNotAPair

--- Builtin ---
isEq :: Register -> [Tree] -> EvaluatedValue
isEq reg [left, right]
  | evaluateValue (Context (reg, left)) == evaluateValue (Context (reg, right)) = ValueTrue
  | otherwise     = ValueFalse
isEq _    _              = throw InvalidNumberOfArguments

--- Builtin ---
isAtom :: Register -> [Tree] -> EvaluatedValue
isAtom reg [arg] = isAtom' $ evaluateValue (Context (reg, arg))
isAtom _    _    = throw InvalidNumberOfArguments

isAtom' :: EvaluatedValue -> EvaluatedValue
isAtom' (List _)  = ValueFalse
isAtom' _         = ValueTrue

--- Builtin Arithmetic ---
add :: Register -> [Tree] -> EvaluatedValue
add = arithmetic (+)

sub :: Register -> [Tree] -> EvaluatedValue
sub = arithmetic (-)

multiplication :: Register -> [Tree] -> EvaluatedValue
multiplication = arithmetic (*)

arithmetic :: (Float -> Float -> Float) -> Register -> [Tree] -> EvaluatedValue
arithmetic _    _   []            = ValueNumber 0
arithmetic _    reg [value]       = checkIsNum $ evaluateValue (Context (reg, value))
arithmetic func reg (value : xs)  = arithmetic' func (evaluateValue (Context (reg, value))) $ arithmetic func reg xs

checkIsNum :: EvaluatedValue -> EvaluatedValue
checkIsNum value@(ValueNumber _)  = value
checkIsNum _                      = throw ArgumentIsNotNumber

arithmetic' :: (Float -> Float -> Float) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic' func (ValueNumber a) (ValueNumber b)  = ValueNumber $ a `func` b
arithmetic' _    _               _                = throw ArgumentIsNotNumber

--- Builtin Arithmetic 2 ---
divProcedure :: Register -> [Tree] -> EvaluatedValue
divProcedure = arithmetic2 (/)

modProcedure :: Register -> [Tree] -> EvaluatedValue
modProcedure = arithmetic2 mod'

arithmetic2 :: (Float -> Float -> Float) -> Register -> [Tree] -> EvaluatedValue
arithmetic2 func reg [left, right] = arithmetic2' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
arithmetic2 _    _   _             = throw InvalidNumberOfArguments

arithmetic2' :: (Float -> Float -> Float) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
arithmetic2' func (ValueNumber left) (ValueNumber right) = ValueNumber $ left `func` right
arithmetic2' _    _                  _                   = throw ArgumentIsNotNumber

--- Builtin Arithmetic 2 ---
ltProcedure :: Register -> [Tree] -> EvaluatedValue
ltProcedure = compareProcedure (<)

lteProcedure :: Register -> [Tree] -> EvaluatedValue
lteProcedure = compareProcedure (<=)

gtProcedure :: Register -> [Tree] -> EvaluatedValue
gtProcedure = compareProcedure (>)

gteProcedure :: Register -> [Tree] -> EvaluatedValue
gteProcedure = compareProcedure (>=)

compareProcedure :: (Float -> Float -> Bool) -> Register -> [Tree] -> EvaluatedValue
compareProcedure func reg [left, right] = compareProcedure' func (evaluateValue (Context (reg, left))) (evaluateValue (Context (reg, right)))
compareProcedure _    _   _             = throw InvalidNumberOfArguments

compareProcedure' :: (Float -> Float -> Bool) -> EvaluatedValue -> EvaluatedValue -> EvaluatedValue
compareProcedure' func (ValueNumber left) (ValueNumber right)
                                          | left `func` right = ValueTrue
                                          | otherwise         = ValueFalse
compareProcedure' _    _                  _                   = throw ArgumentIsNotNumber

--- Builtin ---
quote :: Register -> [Tree] -> EvaluatedValue
quote reg [Empty] = ValueNil
quote reg [Node (Id (RegisterId str)) args] = createList reg $ Leaf (Symbol str) : args
quote reg [Node (IdToGenerate tree)   args] = createList reg $ tree : args
quote reg [Leaf (Number         n)]         = ValueNumber n
quote reg [Leaf (Symbol         str)]       = ValueName str
quote reg [Leaf (UnCreatedList  list)]      = List (ValueName "quote", createList reg list)
quote _    _              = throw InvalidNumberOfArguments

--- Builtin ---
lambda :: Register -> [Tree] -> EvaluatedValue
lambda reg [argsName, body] = createProcedureFromNode argsName body
lambda _    _               = throw InvalidNumberOfArguments

--- Builtin ---
letProcedure :: Register -> [Tree] -> EvaluatedValue
letProcedure reg [args, body] = evaluateValue (Context (regInsertRange2 reg $ getLetArgs reg args, body))
letProcedure _    _           = throw InvalidNumberOfArguments

getLetArgs :: Register -> Tree -> ([RegisterId], [EvaluatedValue])
getLetArgs reg (Node (IdToGenerate firstArg) args)  = getLetArgs' reg $ firstArg : args
getLetArgs _    _                                   = throw $ InvalidSyntax "missing bound value definition"

getLetArgs' :: Register -> [Tree] -> ([RegisterId], [EvaluatedValue])
getLetArgs' reg (Node (Id id) [value] : xs)  = getLetArgs'' reg xs id $ evaluateValue (Context (reg, value))
getLetArgs' _    _                           = throw $ InvalidSyntax "invalid let argument(s)"

getLetArgs'' :: Register -> [Tree] -> RegisterId -> EvaluatedValue -> ([RegisterId], [EvaluatedValue])
getLetArgs'' reg args id value = let (ids, values) = getLetArgs' reg args in (id : ids, value : values)

--- Builtin ---
cond :: Register -> [Tree] -> EvaluatedValue
cond reg (Node (Id (RegisterId "#t")) [arg] : _)  = evaluateValue (Context (reg, arg))
cond reg (Node (Id (RegisterId "#f")) [_]   : xs) = cond reg xs
cond reg (Node id@(IdToGenerate _)    [arg] : xs) = cond reg (Node (Id (generateId reg id)) [arg] : xs)
cond _   []                                       = NoValue
cond _   _                                        = throw InvalidNumberOfArguments

generateId :: Register -> NodeFirstElement -> RegisterId
generateId _    (Id id)           = id
generateId reg  (IdToGenerate id) = generateId' $ evaluateValue (Context (reg, id))

generateId' :: EvaluatedValue -> RegisterId
generateId' (ValueName str) = RegisterId str
generateId' ValueFalse      = RegisterId "#f"
generateId' ValueTrue       = RegisterId "#t"
generateId' _               = throw NotAProcedure
