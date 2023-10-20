{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[]" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
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
import Data.Char

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
          | TernaryIf ADT ADT ADT
          | AdtConst ADT ADT
          | Variable String
          | Block ADT
          | If ADT ADT ADT
          | Else ADT
          | ConstList [ADT]
          | StatementsList [ADT]
          | Function String ADT
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
adtValue = parseAdtInt <|> parseAdtString <|> parseAdtBoolean <|> parseAdtVariable <|> parseAdtList


-- >>> parse parseAdtList "[1,2,3]"
-- Result >< List [Integer 1,Integer 2,Integer 3]
-- >>> parse parseAdtList "[true,false,1]"
-- Result >< List [Boolean True,Boolean False,Integer 1]

-- >>> parse parseAdtList "[\"string\",\"hello\"]"
-- Result >< List [String "string",String "hello"]

parseAdtList :: Parser ADT
parseAdtList = charTok '[' *> (List <$> adtValue `sepBy` commaTok) <* charTok ']'

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

parseAdtTernaryIf :: Parser (ADT -> ADT -> ADT -> ADT)
parseAdtTernaryIf = charTok '?' >> pure TernaryIf

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
--   bracbracketBinaryExprketExpr :: Parser a_auQwk[sk:1]
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
-- Result >< TernaryIf (Integer 1) (Integer 2) (Integer 3)
bracketTernaryExpr :: Parser ADT
bracketTernaryExpr =
  bracketBinaryExpr <|> do
    spaces
    charTok '('
    spaces
    e1 <- bracketBinaryExpr
    spaces
    ternaryOp <- parseAdtTernaryIf
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

-- >>> parse parseExerciseA "(((true)))"
-- Result >< Boolean True
parseExerciseA :: Parser ADT
parseExerciseA = 
  bracketBinaryExpr <|> bracketTernaryExpr <|> do
    spaces
    charTok '('
    spaces
    e1 <- parseExerciseA
    spaces
    charTok ')'
    spaces
    pure e1

prettyPrintList :: [ADT] -> String
prettyPrintList [] = "[]"
prettyPrintList (x:xs) = "[" ++ prettyPrintExerciseA x ++ prettyPrintRest xs
  where
    prettyPrintRest [] = "]"
    prettyPrintRest (y:ys) = ", " ++ prettyPrintExerciseA y ++ prettyPrintRest ys

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA adt = case adt of
  String s -> "\" ++ s ++ \""
  Integer i -> show i
  Boolean True -> "true"
  Boolean False -> "false"
  List l -> prettyPrintList l
  Variable variable -> variable
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
  TernaryIf a b c -> prettyPrintExerciseA a ++ " ? " ++ prettyPrintExerciseA b ++ " : " ++ prettyPrintExerciseA c
  Empty -> "You must write a pretty printer!"

-- | Exercise B

parseAdtConst :: Parser (ADT -> ADT -> ADT)
parseAdtConst = stringTok "const" >> pure AdtConst

-- >>> parse parseAdtVariable "A (1);"
-- Result >(1)< Variable "A"
parseAdtVariable :: Parser ADT
parseAdtVariable = do
  spaces
  variable <- many (satisfy (\c -> isAlphaNum c || isAlpha c || isDigit c || c == '_'))
  spaces
  case variable of
    "" -> empty
    _  -> pure $ Variable variable

-- >>> parse multipleAdtConstExpr "const fc1 = parseInt(\"1234\");"
-- Result >< ConstList [AdtConst (Variable "aVariable") (Integer 4)]
-- >>> parse multipleAdtConstExpr "const a2_3aBcD=1; const a2_3aBcD1=1;   const    b2 =2   ;"
-- Result >< ConstList [AdtConst (Variable "a2_3aBcD") (Integer 1),AdtConst (Variable "a2_3aBcD1") (Integer 1),AdtConst (Variable "b2") (Integer 2)]
adtContExpr :: Parser ADT
adtContExpr = do
    spaces
    constOp <- parseAdtConst
    spaces
    variable <- parseAdtVariable
    spaces
    charTok '='
    spaces
    value <- parseExerciseA
    spaces
    pure $ constOp variable value

multipleAdtConstExpr :: Parser ADT
multipleAdtConstExpr = ConstList <$> (sepBy adtContExpr (charTok ';') <* charTok ';')

-- >>> parse parseAdtBlock "{}"
-- Result >< Block Empty
-- >>> parse parseAdtBlock "{const a = 1; const b = 2; const c = 3;}"
-- Result >< Block (ConstList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Integer 2),AdtConst (Variable "c") (Integer 3)])
-- >>> parse parseAdtBlock "{const a = 1; const b = (a + 1); }"
-- Result >< Block (ConstList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])
parseAdtBlock :: Parser ADT
parseAdtBlock = do
    spaces
    charTok '{'
    spaces
    content <- multipleAdtConstExpr <|> pure Empty
    spaces
    charTok '}'
    spaces
    pure $ Block content



parseAdtIf :: Parser (ADT -> ADT -> ADT -> ADT)
parseAdtIf = stringTok "if" >> pure If

parseAdtElse :: Parser (ADT -> ADT)
parseAdtElse = stringTok "else" >> pure Else
-- >>> parse parseConditional "if (true) {}"
-- Result >< If (Boolean True) (Block Empty) Empty
-- >>> parse parseConditional "if ( (true && false) ){const a = 1;const b = (a + 1);}"
-- Result >< If (And (Boolean True) (Boolean False)) (Block (List [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty
-- >>> parse parseConditional "if ((true && false)){const a = 1; const b = 1;}"
-- Result >< If (And (Boolean True) (Boolean False)) (Block (List [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Integer 1)])) Empty
parseConditionalIfElse :: Parser ADT
parseConditionalIfElse = do
      spaces
      ifOp <- parseAdtIf
      spaces
      charTok '('
      spaces
      condition <- parseExerciseA
      spaces
      charTok ')'
      spaces
      ifBlock <- parseAdtBlock
      spaces
      elseOp <- parseAdtElse
      spaces
      elseBlock <- parseAdtBlock
      spaces
      pure $ ifOp condition ifBlock (elseOp elseBlock)

parseConditionalIf :: Parser ADT
parseConditionalIf = do
      spaces
      ifOp <- parseAdtIf
      spaces
      charTok '('
      spaces
      condition <- parseExerciseA
      spaces
      charTok ')'
      spaces
      ifBlock <- parseAdtBlock
      spaces
      pure $ ifOp condition ifBlock Empty

parseConditional :: Parser ADT
parseConditional = parseConditionalIfElse <|> parseConditionalIf <|> multipleAdtConstExpr
-- >>> parse parseExerciseB "{    }"
-- Result >< Block Empty
-- >>> parse parseExerciseB "{ const variableA = 1; const variableB = 2; }"
-- Result >< Block (ConstList [AdtConst (Variable "variableA") (Integer 1),AdtConst (Variable "variableB") (Integer 2)])
-- >>> parse parseExerciseB "const    list =[2,3,4]   ;"
-- Result >< ConstList [AdtConst (Variable "list") (List [Integer 2,Integer 3,Integer 4])]

-- >>> parse parseExerciseB "{ const variableA = 1; const variableB = 2; }"
-- Result >< [Block (ConstList [AdtConst (Variable "variableA") (Integer 1),AdtConst (Variable "variableB") (Integer 2)])]

-- >>> parse parseExerciseB "if ( (true && false) ){const a = 1;const b = (a + 1);} const if2 = (true ? 1 : 4);if ( (true && false) ){const a = 1;const b = (a + 1);}"
-- Result >< [If (And (Boolean True) (Boolean False)) (Block (ConstList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty,ConstList [AdtConst (Variable "if2") (TernaryIf (Boolean True) (Integer 1) (Integer 4))],If (And (Boolean True) (Boolean False)) (Block (ConstList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty]

-- >>> parse parseExerciseB "if ( (!false) ) { const a = 1; const b = (a + 1); } else { const a = (true ? 1 : 2); const b = (a - 1); } const if2 = (true ? 1 : 4);"
-- Result >< StatementsList [If (Not (Boolean False)) (Block (ConstList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) (Else (Block (ConstList [AdtConst (Variable "a") (TernaryIf (Boolean True) (Integer 1) (Integer 2)),AdtConst (Variable "b") (Minus (Variable "a") (Integer 1))]))),ConstList [AdtConst (Variable "if2") (TernaryIf (Boolean True) (Integer 1) (Integer 4))]]
parseExerciseB :: Parser ADT
parseExerciseB = StatementsList <$> many(multipleAdtConstExpr <|> parseAdtBlock <|> parseConditional)
    
prettyPrintBlock :: [ADT] -> String
prettyPrintBlock [] = "[]"
prettyPrintBlock (x:xs) = "{\n " ++ prettyPrintExerciseB x ++ prettyPrintRest xs
  where
    prettyPrintRest [] = ";\n}"
    prettyPrintRest (y:ys) = ";\n " ++ prettyPrintExerciseB y ++ prettyPrintRest ys

prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB adt = case adt of
  AdtConst variable value -> "const " ++ prettyPrintExerciseA variable ++ " = " ++ prettyPrintExerciseA value
  ConstList l ->  foldr (\x acc -> prettyPrintExerciseB x ++ ";" ++ acc) "" l
  Block Empty -> "{}"
  Block (ConstList l) -> prettyPrintBlock l
  If a b Empty -> "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintExerciseB b
  If a b (Else c) -> "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintExerciseB b ++ " else " ++ prettyPrintExerciseB c
  StatementsList l -> foldr (\x acc -> prettyPrintExerciseB x ++ "\n" ++ acc) "" l
  _ -> "You must write a pretty printer!"

-- | Exercise C

-- >>> parse parseAdtVariable "parseInt(\"1234\");"
-- Result >("1234");< Variable "parseInt"
parseAdtFunction :: Parser ADT
parseAdtFunction =  do
    spaces
    variable <- many (satisfy (\c -> isAlphaNum c || isAlpha c || isDigit c || c == '_'))
    spaces
    parameter <- parseExerciseA
    case variable of
      "" -> empty
      _  -> pure $ Function variable parameter
-- This function should determine if the given code is a tail recursive function
isTailRecursive :: String -> Bool
isTailRecursive _ = False

parseExerciseC :: Parser ADT
parseExerciseC = parseAdtFunction

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC adt = case adt of
    Function variable parameter -> variable ++ "(" ++ prettyPrintExerciseA parameter ++ ")"
    _ -> "You must write a pretty printer!"
