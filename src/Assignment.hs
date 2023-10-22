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
          | BlockList [ADT]
          | StatementsList [ADT]
          | FuncHead String ADT
          | ParameterList [ADT]
          | Function ADT ADT
          | Return ADT
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
adtValue = parseAdtInt <|> parseAdtString <|> parseAdtBoolean <|> parseAdtFuncHead <|> parseAdtVariable <|> parseAdtList


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

-- >>> parse parseExerciseA "(parse(5))"
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

prettyPrintParameterList :: [ADT] -> String
prettyPrintParameterList [] = ""
prettyPrintParameterList (x:xs) = prettyPrintExerciseA x ++ prettyPrintRest xs
  where
    prettyPrintRest [] = ""
    prettyPrintRest (y:ys) = ", " ++ prettyPrintExerciseA y ++ prettyPrintRest ys

prettyPrintExerciseA :: ADT -> String
prettyPrintExerciseA adt = case adt of
  String s -> "\"" ++ s ++ "\""
  Integer i -> show i
  Boolean True -> "true"
  Boolean False -> "false"
  List l -> prettyPrintList l
  Variable variable -> variable
  FuncHead variable parameter -> variable ++ "(" ++ prettyPrintExerciseA parameter ++ ")"
  ParameterList l -> prettyPrintParameterList l
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
-- >>> parse adtContExpr "const fc2 = parseInt(\"12\");"
-- Result >;< AdtConst (Variable "fc2") (FuncHead "parseInt" (ParameterList [String "12"]))

-- >>> parse multipleAdtConstExpr "const fc2 = parseInt(\"12\", 11);"
-- Result >< BlockList [AdtConst (Variable "fc2") (FuncHead "parseInt" (ParameterList [String "12",Integer 11]))]
-- >>> parse multipleAdtConstExpr "const a2_3aBcD=1; const a2_3aBcD1=1;   const    b2 =2   ;"
-- Result >< BlockList [AdtConst (Variable "a2_3aBcD") (Integer 1),AdtConst (Variable "a2_3aBcD1") (Integer 1),AdtConst (Variable "b2") (Integer 2)]
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
-- >>> parse multipleAdtConstExpr "const a = 1; return ((c + b) + parseInt(\"123\"));"
-- Result >< BlockList [AdtConst (Variable "a") (Integer 1),Return (Plus (Plus (Variable "c") (Variable "b")) (FuncHead "parseInt" (ParameterList [String "123"])))]
-- >>> parse multipleAdtConstExpr "return factorial((n - 1), (acc * n));"
-- Unexpected character: "("
multipleAdtConstExpr :: Parser ADT
multipleAdtConstExpr = BlockList <$> (sepBy (adtContExpr <|> adtReturnExpr) (charTok ';') <* charTok ';')
-- Result >return factorial((n - 1), (acc * n));< If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [Return (Variable "acc")])) Empty

-- >>> parse parseAdtBlock "{}"
-- Result >< Block (BlockList [])
-- >>> parse parseAdtBlock "{const a = 1; const b = 2; const c = 3;}"
-- Result >< Block (BlockList [BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Integer 2),AdtConst (Variable "c") (Integer 3)]])
-- >>> parse parseAdtBlock "{const a = 1; const b = (a + 1); }"
-- Result >< Block (BlockList [BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))]])
-- >>> parse parseAdtBlock "{if (((n < 0) || (n === 0))) { return acc; }return factorial((n - 1), (acc * n));}"
-- Result >< Block (BlockList [If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [BlockList [Return (Variable "acc")]])) Empty,BlockList [Return (FuncHead "factorial" (ParameterList [Minus (Variable "n") (Integer 1),Times (Variable "acc") (Variable "n")]))]])
parseAdtBlock :: Parser ADT
parseAdtBlock = do
    spaces
    charTok '{'
    spaces
    content <- multipleConditionalBlock <|> pure Empty
    spaces
    charTok '}'
    spaces
    pure $ Block content



parseAdtIf :: Parser (ADT -> ADT -> ADT -> ADT)
parseAdtIf = stringTok "if" >> pure If

parseAdtElse :: Parser (ADT -> ADT)
parseAdtElse = stringTok "else" >> pure Else
-- >>> parse parseConditional "if (((n < 0) || (n === 0))) { return pprev; }"
-- Result >< If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [Return (Variable "pprev")])) Empty
-- >>> parse parseConditional "if ( (true && false) ){const a = 1;const b = (a + 1);}"
-- Result >< If (And (Boolean True) (Boolean False)) (Block (BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty
-- >>> parse parseConditional "if ((true && false)){const a = 1; const b = 1;}"
-- Result >< If (And (Boolean True) (Boolean False)) (Block (BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Integer 1)])) Empty
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

-- >>> parse parseConditional "if (((n < 0) || (n === 0))) { return acc; }return factorial((n - 1), (acc * n));"
-- Result >return factorial((n - 1), (acc * n));< If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [Return (Variable "acc")])) Empty
parseConditional :: Parser ADT
parseConditional = parseConditionalIfElse <|> parseConditionalIf

-- >>> parse multipleConditionalBlock "if (((n < 0) || (n === 0))) { return pprev; }if ((n === 1)) { return prev; }1`"
-- Result >< BlockList [If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [Return (Variable "pprev")])) Empty,If (Eq (Variable "n") (Integer 1)) (Block (BlockList [Return (Variable "prev")])) Empty,BlockList [Return (FuncHead "fibonacci" (ParameterList [Minus (Variable "n") (Integer 1),Variable "prev",Plus (Variable "pprev") (Variable "prev")]))]]
multipleConditionalBlock :: Parser ADT
multipleConditionalBlock = BlockList <$> many (parseConditional <|> multipleAdtConstExpr)

-- >>> parse adtReturnExpr "return a"
-- >>> parse parseExerciseB "{ const variableA = 1; const variableB = 2; }"
-- Result >< StatementsList [Block (BlockList [AdtConst (Variable "variableA") (Integer 1),AdtConst (Variable "variableB") (Integer 2)])]
-- >>> parse parseExerciseB "const    list =[2,3,4]   ;"
-- Result >< StatementsList [BlockList [AdtConst (Variable "list") (List [Integer 2,Integer 3,Integer 4])]]

-- >>> parse parseExerciseB "{ const variableA = 1; const variableB = 2; }"
-- Result >< StatementsList [Block (BlockList [AdtConst (Variable "variableA") (Integer 1),AdtConst (Variable "variableB") (Integer 2)])]

-- >>> parse parseExerciseB "if ( (true && false) ){const a = 1;const b = (a + 1);} const if2 = (true ? 1 : 4);if ( (true && false) ){const a = 1;const b = (a + 1);}"
-- Result >< StatementsList [If (And (Boolean True) (Boolean False)) (Block (BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty,BlockList [AdtConst (Variable "if2") (TernaryIf (Boolean True) (Integer 1) (Integer 4))],If (And (Boolean True) (Boolean False)) (Block (BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) Empty]

-- >>> parse parseExerciseB "if ( (!false) ) { const a = 1; const b = (a + 1); } else { const a = (true ? 1 : 2); const b = (a - 1); } const if2 = (true ? 1 : 4);"
-- Result >< StatementsList [If (Not (Boolean False)) (Block (BlockList [AdtConst (Variable "a") (Integer 1),AdtConst (Variable "b") (Plus (Variable "a") (Integer 1))])) (Else (Block (BlockList [AdtConst (Variable "a") (TernaryIf (Boolean True) (Integer 1) (Integer 2)),AdtConst (Variable "b") (Minus (Variable "a") (Integer 1))]))),BlockList [AdtConst (Variable "if2") (TernaryIf (Boolean True) (Integer 1) (Integer 4))]]
parseExerciseB :: Parser ADT
parseExerciseB = StatementsList <$> many (multipleAdtConstExpr <|> parseAdtBlock <|> parseConditional)


prettyPrintBlock :: [ADT] -> Bool -> String
prettyPrintBlock [] _ = "{}"
prettyPrintBlock adts False = "{" ++ prettyPrintBlock' adts 1 ++ "\n}"
prettyPrintBlock adts True = "{\n  while (true) {" ++ prettyPrintBlock' adts 2 ++ "\n  }\n}"

prettyPrintBlock' :: [ADT] -> Int -> String
prettyPrintBlock' [] _ = ""
prettyPrintBlock' (x:xs) indent = case x of
  BlockList l -> prettyPrintBlock' l indent ++ prettyPrintBlock' xs indent
  Block (BlockList l) -> "{" ++ prettyPrintBlock' l (indent + 1) ++ prettyPrintBlock' xs indent  ++ "\n" ++ replicate (indent * 2) ' ' ++ "}"
  If a b Empty -> "\n" ++ replicate (indent * 2) ' ' ++ "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintBlock' [b] indent ++ prettyPrintBlock' xs indent
  If a b (Else c) -> "\n" ++ replicate (indent * 2) ' ' ++ "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintBlock' [b] indent ++ " else " ++ prettyPrintBlock' [c] indent ++ prettyPrintBlock' xs indent
  _ -> "\n" ++ replicate (indent * 2) ' ' ++ prettyPrintExerciseB x ++ ";" ++ prettyPrintBlock' xs indent



prettyPrintExerciseB :: ADT -> String
prettyPrintExerciseB adt = case adt of
  AdtConst variable value -> "const " ++ prettyPrintExerciseA variable ++ " = " ++ prettyPrintExerciseA value
  BlockList l ->  foldr (\x acc -> prettyPrintExerciseB x ++ ";" ++ acc) "" l
  Block Empty -> "{}"
  Block (BlockList l) -> prettyPrintBlock l False
  If a b Empty -> "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintExerciseB b
  If a b (Else c) -> "if (" ++ prettyPrintExerciseA a ++ ") " ++ prettyPrintExerciseB b ++ " else " ++ prettyPrintExerciseB c
  StatementsList l -> foldr (\x acc -> prettyPrintExerciseB x ++ "\n" ++ acc) "" l
  Return value -> "return " ++ prettyPrintExerciseA value
  _ -> "You must write a pretty printer!"

-- | Exercise C

-- >>> parse parseAdtFuncHead "parseInt(\"1234\", 1);"
-- Result >;< FuncHead "parseInt" (ParameterList [String "1234",Integer 1])
-- >>> parse parseAdtFuncHead "factorial((n - 1), (acc * n));"
-- Result >;< FuncHead "factorial" (ParameterList [Minus (Variable "n") (Integer 1),Times (Variable "acc") (Variable "n")])
parseAdtFuncHead :: Parser ADT
parseAdtFuncHead =  do
    spaces
    variable <- many (satisfy (\c -> isAlphaNum c || isAlpha c || isDigit c || c == '_'))
    spaces
    parameter <- charTok '(' *> (ParameterList <$> parseExerciseA `sepBy` commaTok) <* charTok ')'
    case variable of
      "" -> empty
      _  -> pure $ FuncHead variable parameter

parseAdtFunction :: Parser (ADT -> ADT -> ADT)
parseAdtFunction = stringTok "function" >> pure Function

parseAdtReturn :: Parser (ADT -> ADT)
parseAdtReturn = stringTok "return" >> pure Return
-- >>> parse parseExerciseA "(parseInt(\"123\"))"
-- Unexpected character: "("

-- >>> parse adtReturnExpr "return ((a + b) + parseInt(\"123\"));"
-- Result >;< Return (Plus (Plus (Variable "a") (Variable "b")) (FuncHead "parseInt" (ParameterList [String "123"])))
-- >>> parse adtReturnExpr "return factorial((n - 1), (acc * n));"
-- Result >;< Return (FuncHead "factorial" (ParameterList [Minus (Variable "n") (Integer 1),Times (Variable "acc") (Variable "n")]))
adtReturnExpr :: Parser ADT
adtReturnExpr = do
  spaces
  returnOp <- parseAdtReturn
  spaces
  value <- parseExerciseA
  spaces
  pure $ returnOp value
-- >>> parse adtFunctionExpr "function a(x, y, z) {const b = 3;}"
-- Result >< Function (FuncHead "a" (ParameterList [Variable "x",Variable "y",Variable "z"])) (Block (BlockList [AdtConst (Variable "b") (Integer 3)]))
-- >>> parse adtFunctionExpr "function someNumber2(c, b) {const a = 1; return ((a + c) + parseInt(\"123\"));}"
-- Result >< Function (FuncHead "someNumber2" (ParameterList [Variable "c",Variable "b"])) (Block (BlockList [AdtConst (Variable "a") (Integer 1),Return (Plus (Plus (Variable "a") (Variable "c")) (FuncHead "parseInt" (ParameterList [String "123"])))]))

-- >>> parse adtFunctionExpr "function fibonacci(n, pprev, prev) {if (((n < 0) || (n === 0))) { return pprev; };}"
-- Result >< Function (FuncHead "fibonacci" (ParameterList [Variable "n",Variable "pprev",Variable "prev"])) (Block (BlockList [If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [Return (Variable "pprev")])) Empty]))
adtFunctionExpr :: Parser ADT
adtFunctionExpr = do
  spaces
  functionOp <- parseAdtFunction
  spaces
  functionHead <- parseAdtFuncHead
  spaces
  functionBlock <- parseAdtBlock
  spaces
  pure $ functionOp functionHead functionBlock

-- This function should determine if the given code is a tail recursive function
-- >>> isTailRecursive "function tail(n) {if ((n > 0)) {return tail((n-1)); }}"
-- False
-- >>> isTailRecursive "function factorial(n, acc) {if (((n < 0) || (n === 0))) {const b=2; return acc; }return factorial((n - 1), (acc * n));}"
-- True
-- >>> isTailRecursive "function fibonacci(n, pprev, prev) {if (((n < 0) || (n === 0))) { return pprev; }if ((n === 1)) { return prev; }return fibonacci((n - 1), prev, (pprev + prev));}"
-- True
-- >>> isTailRecursive "function fibonacci(n) {if (((n < 0) || (n === 0))) { return 0; }if ((n === 1)) { return 1; }return (fibonacci((n - 1)) + fibonacci((n - 2))); }"
-- False
isTailRecursiveAdt :: ADT -> Bool
isTailRecursiveAdt adt = case adt of
  Function (FuncHead functionName (ParameterList params)) (Block (BlockList l)) -> normalReturnCheck l && atEndReturnCheck l [Empty] False && recursiveCheck l [Empty] functionName (length params)
  _ -> False

isTailRecursive :: String -> Bool
isTailRecursive function = case parse parseExerciseC function of
  Result _ (Function (FuncHead functionName (ParameterList params)) (Block (BlockList l))) -> normalReturnCheck l && atEndReturnCheck l [Empty] False && recursiveCheck l [Empty] functionName (length params)
  _ -> False

normalReturnCheck :: [ADT] -> Bool
normalReturnCheck [] = False
normalReturnCheck (x:xs) = case x of
  If _ (Block (BlockList l))  _ -> normalReturnCheck l
  AdtConst _ _-> True
  Block (BlockList l) -> normalReturnCheck l
  BlockList l -> normalReturnCheck l
  Return( FuncHead functionName (ParameterList params)) -> False
  Return value -> True
  _ -> False

atEndReturnCheck :: [ADT] -> [ADT] -> Bool -> Bool
atEndReturnCheck [] _ True = True
atEndReturnCheck [] _ False = False
atEndReturnCheck (x:xs) [Empty] _ = atEndReturnCheck (x:xs) xs False
atEndReturnCheck (x:xs) remaining _ = case x of
  If _ (Block (BlockList l)) _ -> atEndReturnCheck l remaining False
  Block (BlockList l) -> atEndReturnCheck l remaining False
  AdtConst _ _-> atEndReturnCheck remaining [Empty] False
  BlockList l -> atEndReturnCheck l remaining False
  Return (FuncHead functionName (ParameterList params)) -> atEndReturnCheck xs [Empty] True
  Return value -> atEndReturnCheck remaining [Empty] False
  _ -> False

recursiveCheck :: [ADT] -> [ADT] -> String -> Int -> Bool
recursiveCheck [] _ _ _ = False
recursiveCheck (x:xs) [Empty] functionName paramNum = recursiveCheck (x:xs) xs functionName paramNum
recursiveCheck (x:xs) remaining functionName paramNum = case x of
  If _ (Block (BlockList l))  _ -> recursiveCheck l remaining functionName paramNum
  Block (BlockList l) -> recursiveCheck l remaining functionName paramNum
  AdtConst _ _-> recursiveCheck remaining [Empty] functionName paramNum
  BlockList l -> recursiveCheck l remaining functionName paramNum
  Return( FuncHead name (ParameterList params)) -> name == functionName && length params == paramNum
  Return value -> recursiveCheck remaining [Empty] functionName paramNum
  _ -> False


-- >>> parse parseExerciseC "const fc2 = parseInt(\"12\", 10);"
-- Result >< BlockList [AdtConst (Variable "fc2") (FuncHead "parseInt" (ParameterList [String "12",Integer 10]))]
-- >>> parse parseExerciseC "function someNumber2(a, b) {const a = 1; return ((a + b) + parseInt(\"123\"));}"
-- Result >< Function (FuncHead "someNumber2" (ParameterList [Variable "a",Variable "b"])) (Block (BlockList [BlockList [AdtConst (Variable "a") (Integer 1),Return (Plus (Plus (Variable "a") (Variable "b")) (FuncHead "parseInt" (ParameterList [String "123"])))]]))
-- >>> parse parseExerciseC "function fibonacci(n, pprev, prev) {if (((n < 0) || (n === 0))) { return pprev; }if ((n === 1)) { return prev; }return fibonacci((n - 1), prev, (pprev + prev));}"
-- Result >< Function (FuncHead "fibonacci" (ParameterList [Variable "n",Variable "pprev",Variable "prev"])) (Block (BlockList [If (Or (Lt (Variable "n") (Integer 0)) (Eq (Variable "n") (Integer 0))) (Block (BlockList [BlockList [Return (Variable "pprev")]])) Empty,If (Eq (Variable "n") (Integer 1)) (Block (BlockList [BlockList [Return (Variable "prev")]])) Empty,BlockList [Return (FuncHead "fibonacci" (ParameterList [Minus (Variable "n") (Integer 1),Variable "prev",Plus (Variable "pprev") (Variable "prev")]))]]))
parseExerciseC :: Parser ADT
parseExerciseC =  multipleAdtConstExpr <|> adtFunctionExpr

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC adt = if isTailRecursiveAdt adt 
  then tailRecursivePrettyPrint adt
  else normalPrettyPrint adt

  
normalPrettyPrint :: ADT -> String
normalPrettyPrint adt = case adt of
  AdtConst variable value -> "const " ++ prettyPrintExerciseA variable ++ " = " ++ prettyPrintExerciseA value
  BlockList l -> foldr (\x acc -> normalPrettyPrint x ++ ";" ++ acc) "" l
  Block Empty -> "{}"
  Block (BlockList l) -> prettyPrintBlock l False
  Function functionHead functionBlock -> "function " ++ prettyPrintExerciseA functionHead ++ " " ++ normalPrettyPrint functionBlock
  Return value -> "return " ++ prettyPrintExerciseA value
  _ -> "You must write a pretty printer!"

tailRecursivePrettyPrint :: ADT -> String
tailRecursivePrettyPrint adt = case adt of
  AdtConst variable value -> "const " ++ prettyPrintExerciseA variable ++ " = " ++ prettyPrintExerciseA value
  BlockList l -> foldr (\x acc -> tailRecursivePrettyPrint x ++ ";" ++ acc) "" l
  Block Empty -> "{}"
  Block (BlockList l) -> prettyPrintBlock l True
  Function functionHead functionBlock -> "function " ++ prettyPrintExerciseA functionHead ++ " " ++ tailRecursivePrettyPrint functionBlock 
  Return value -> "return " ++ prettyPrintExerciseA value
  _ -> "You must write a pretty printer!"
