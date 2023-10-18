{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[]" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You may, and are highly encouraged, to create your functions.
module Assignment where

import Instances
import Parser
import Control.Applicative

data ADT = String String
          | Integer Int
          | Boolean Bool
          | List [ADT]
          | Not ADT
          | And ADT ADT
          | Or ADT ADT
          | Plus ADT ADT
          | Minus ADT ADT
          | Times ADT ADT
          | Power ADT ADT
          | Divide ADT ADT
          | Eq ADT ADT
          | Neq ADT ADT
          | Gt ADT ADT
          | Lt ADT ADT
          | If ADT ADT ADT
          | Empty
  deriving (Eq, Show)

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = p <:> many (sep *> p)
  where
    -- (Optional) Join two parsers into a list
    (<:>) :: Parser a -> Parser [a] -> Parser [a]
    (<:>) = liftA2 (:)

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = sepBy1 p sep <|> pure []

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> pure a

-- | Exercise A


-- >>> parse parseAdtInt "123"
-- Result >< Integer 123

parseAdtInt :: Parser ADT
parseAdtInt = Integer <$> (spaces *> int <* spaces)

-- >>> parse parseString " \"abcsddsafddc?c\"  "
-- Result >< "abcsddsafddc?c"

-- >>> parse parseString "\"FIT2102 is so fun!\""
-- Result >< "FIT2102 is so fun!"
parseString :: Parser String
parseString = spaces *> charTok '\"' *> many (isNot '\"') <* charTok '\"' <* spaces

-- >>> parse parseAdtString "\"FIT2102 is so fun!\""
-- Result >< String "FIT2102 is so fun!"

parseAdtString :: Parser ADT
parseAdtString = String <$> parseString

-- >>> parse parseBoolean "true"
-- Result >< True
parseBoolean :: Parser Bool
parseBoolean = True <$ string "true" <|> False <$ string "false"

-- >>> parse parseAdtBoolean "true"
-- Result >< Boolean True
parseAdtBoolean :: Parser ADT
parseAdtBoolean = Boolean <$> parseBoolean

adtValue :: Parser ADT
adtValue = parseAdtInt <|> parseAdtString <|> parseAdtBoolean


-- >>> parse parseList "[1,2,3]"
-- Result >< List [Integer 1,Integer 2,Integer 3]
-- >>> parse parseList "[true,false,1]"
-- Result >< List [Boolean True,Boolean False,Integer 1]

-- >>> parse parseList "[\"string\",\"hello\"]"
-- Result >< List [String "string",String "hello"]

parseList :: Parser ADT
parseList = charTok '[' *> (List <$> adtValue `sepBy` commaTok) <* charTok ']'

parseAdtNot :: Parser (ADT -> ADT)
parseAdtNot =  charTok '!' >> pure Not

parseAdtAnd :: Parser (ADT -> ADT -> ADT)
parseAdtAnd = stringTok "&&" >> pure And

parseAdtOr :: Parser (ADT -> ADT -> ADT)
parseAdtOr = stringTok "||" >> pure Or

parseAdtAndOr :: Parser (ADT -> ADT -> ADT)
parseAdtAndOr = parseAdtAnd <|> parseAdtOr

bracketNotBoolean :: Parser ADT
bracketNotBoolean = spaces *> charTok '(' *> parseAdtNot <*> parseAdtBoolean <* charTok ')' <* spaces

parseAdtPlus :: Parser (ADT -> ADT -> ADT)
parseAdtPlus = charTok '+' >> pure Plus

parseAdtMinus :: Parser (ADT -> ADT -> ADT)
parseAdtMinus = charTok '-' >> pure Minus

parseAdtTimes :: Parser (ADT -> ADT -> ADT)
parseAdtTimes = charTok '*' >> pure Times

parseAdtPower :: Parser (ADT -> ADT -> ADT)
parseAdtPower = stringTok "**" >> pure Power

parseAdtDivide :: Parser (ADT -> ADT -> ADT)
parseAdtDivide = charTok '/' >> pure Divide

parseAdtEq :: Parser (ADT -> ADT -> ADT)
parseAdtEq = string "===" >> pure Eq

parseAdtNeq :: Parser (ADT -> ADT -> ADT)
parseAdtNeq = string "!==" >> pure Neq

parseAdtGt :: Parser (ADT -> ADT -> ADT)
parseAdtGt = charTok '>' >> pure Gt

parseAdtLt :: Parser (ADT -> ADT -> ADT)
parseAdtLt = charTok '<' >> pure Lt

parseAdtIf :: Parser (ADT -> ADT -> ADT -> ADT)
parseAdtIf = charTok '?' >> pure If

parseExpr :: Parser (ADT -> ADT -> ADT)
parseExpr = parseAdtAnd <|> parseAdtOr <|> parseAdtPower <|> parseAdtPlus <|> parseAdtMinus <|> parseAdtTimes <|> parseAdtDivide <|> parseAdtEq <|> parseAdtNeq <|> parseAdtGt <|> parseAdtLt

-- >>> parse bracketBinaryExpr "(((!false) && true) || true)"
-- Result >< Or (And (Not (Boolean False)) (Boolean True)) (Boolean True)
-- >>> parse bracketBinaryExpr "((!false) || true)"
-- Result >< Or (Not (Boolean False)) (Boolean True)
-- >>> parse bracketBinaryExpr "((!false) && true)"
-- Result >< And (Not (Boolean False)) (Boolean True)
-- >>> parse bracketBinaryExpr "(!true)"
-- Result >< Not (Boolean True)
-- >>> parse bracketBinaryExpr "(1 + 2)"
-- Result >< Plus (Integer 1) (Integer 2)
-- >>> parse bracketBinaryExpr "(1 - (2 + 5))"
-- Result >< Minus (Integer 1) (Plus (Integer 2) (Integer 5))
-- >>> parse bracbracketBinaryExprketExpr "(1 * (2 + 5))"
-- Variable not in scope:
--   bracbracketBinaryExprketExpr :: Parser a_aHeL[sk:1]
-- >>> parse bracketBinaryExpr "(1 ** (2 + 5))"
-- Result >< Power (Integer 1) (Plus (Integer 2) (Integer 5))
-- >>> parse bracketBinaryExpr "(1 / (2 + 5))"
-- Result >< Divide (Integer 1) (Plus (Integer 2) (Integer 5))
-- >>> parse bracketBinaryExpr "((!true) === 1)"
-- Result >< Eq (Not (Boolean True)) (Integer 1)
-- >>> parse bracketBinaryExpr "(1 !== 2)"
-- Result >< Neq (Integer 1) (Integer 2)
-- >>> parse bracketBinaryExpr "(((2 + 3) * 2) === (4 + (3 * 2)))"
-- Result >< Eq (Times (Plus (Integer 2) (Integer 3)) (Integer 2)) (Plus (Integer 4) (Times (Integer 3) (Integer 2)))
-- >>> parse bracketBinaryExpr "(\"abc\" > 5)"
-- Result >< Gt (String "abc") (Integer 5)
bracketBinaryExpr :: Parser ADT
bracketBinaryExpr =
  bracketNotBoolean <|> adtValue <|> do
    spaces
    charTok '('
    spaces
    e1 <- bracketBinaryExpr
    spaces
    op <- parseExpr
    spaces
    e2 <- bracketBinaryExpr
    spaces
    charTok ')'
    spaces
    pure $ op e1 e2


-- >>> parse bracketTernaryExpr "(1 ? 2 : 3)"
-- Result >< If (Integer 1) (Integer 2) (Integer 3)
bracketTernaryExpr :: Parser ADT
bracketTernaryExpr =
  bracketBinaryExpr <|> do
    spaces
    charTok '('
    spaces
    e1 <- bracketBinaryExpr
    spaces
    ternaryOp <- parseAdtIf
    spaces
    e2 <- bracketBinaryExpr
    spaces
    charTok ':'
    spaces
    e3 <- bracketBinaryExpr
    spaces
    charTok ')'
    spaces
    pure $ ternaryOp e1 e2 e3

parseExerciseA :: Parser ADT
parseExerciseA = bracketBinaryExpr <|> bracketTernaryExpr

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA adt = case adt of
  String s -> "\" ++ s ++ \""
  Integer i -> show i
  Boolean True -> show "true"
  Boolean False -> show "false"
  List l -> "[" ++ foldr (\x acc -> prettyPrintExerciseA x ++ "," ++ acc) "" l ++ "]"
  Not a -> "!" ++ prettyPrintExerciseA a
  And a b -> prettyPrintExerciseA a ++ " && " ++ prettyPrintExerciseA b
  Or a b -> prettyPrintExerciseA a ++ " || " ++ prettyPrintExerciseA b
  Plus a b -> prettyPrintExerciseA a ++ " + " ++ prettyPrintExerciseA b
  Minus a b -> prettyPrintExerciseA a ++ " - " ++ prettyPrintExerciseA b
  Times a b -> prettyPrintExerciseA a ++ " * " ++ prettyPrintExerciseA b
  Power a b -> prettyPrintExerciseA a ++ " ** " ++ prettyPrintExerciseA b
  Divide a b -> prettyPrintExerciseA a ++ " / " ++ prettyPrintExerciseA b
  Eq a b -> prettyPrintExerciseA a ++ " === " ++ prettyPrintExerciseA b
  Neq a b -> prettyPrintExerciseA a ++ " !== " ++ prettyPrintExerciseA b
  Gt a b -> prettyPrintExerciseA a ++ " > " ++ prettyPrintExerciseA b
  Lt a b -> prettyPrintExerciseA a ++ " < " ++ prettyPrintExerciseA b
  If a b c -> prettyPrintExerciseA a ++ " ? " ++ prettyPrintExerciseA b ++ " : " ++ prettyPrintExerciseA c
  Empty -> "You must write a pretty printer!"

-- | Exercise B

parseExerciseB :: Parser ADT
parseExerciseB = pure Empty

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB _ = "You must write a pretty printer!"

-- | Exercise C


-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive _ = False

parseExerciseC :: Parser ADT
parseExerciseC = pure Empty

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC _ = "You must write a pretty printer!"
