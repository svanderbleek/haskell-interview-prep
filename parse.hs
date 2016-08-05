import Text.Parsec
  ((<|>)
  ,parseTest)

import Text.Parsec.String
  (Parser)

import Text.Parsec.Expr 
  (Operator(Infix)
  ,Assoc(AssocLeft)
  ,buildExpressionParser)

import Text.Parsec.Token 
  (makeTokenParser
  ,reservedOp
  ,parens
  ,integer)

import Text.Parsec.Language
  (emptyDef)

data Expression
  = Addition Expression Expression
  | Multiplication Expression Expression
  | Value Integer
  deriving (Show)

tokenParser = makeTokenParser emptyDef

operatorTable =
  [[binaryOperator "*" Multiplication]
  ,[binaryOperator "+" Addition]]

binaryOperator symbol constructor =
  Infix constructorParser AssocLeft
  where
    constructorParser = 
      do
        reservedOp tokenParser symbol
        return constructor

operatorParser = buildExpressionParser operatorTable expressionParser

expressionParser =
  parens tokenParser operatorParser
  <|> Value <$> integer tokenParser

-- want this to also work for "1*1+1*1" :(
main =
  sequence 
    [parseTest expressionParser "(1*1+1*1)"
    ,putStrLn ""
    ,parseTest expressionParser "1*1+1*1"]
