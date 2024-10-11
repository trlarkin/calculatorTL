{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Parser where

import Text.Parsec.Char
import Text.Parsec.String
import MathExpr
import Text.Parsec


-- vvvvvvvvvv Grammar vvvvvvvvvv

pMathExpr :: Parser MathExpr
pMathExpr = pTerm `pAdd` pMathExpr
        <|> pTerm `pSub` pMathExpr
        <|> pTerm
        <?>  "math expression"

pTerm :: Parser MathExpr
pTerm = pExponentiation `pMul` pTerm
    <|> pExponentiation `pDiv` pTerm
    <|> pExponentiation
    <?> "term"

pExponentiation :: Parser MathExpr
pExponentiation = pBasic `pPowerOf` pExponentiation
    <|> pBasic

pBasic :: Parser MathExpr
pBasic = pNumber <|> pVar <|> pEnclosedMath

-- ^^^^^^^^^^ Grammar ^^^^^^^^^^

pEnclosed :: Parser a -> Parser a
pEnclosed p = try (char '(' *> p <* char ')')
          <|> try (char '{' *> p <* char '}')
          <|> try (char '[' *> p <* char ']')

pEnclosedMath :: Parser MathExpr
pEnclosedMath = pEnclosed pMathExpr <?> "parenthesized expression"

pAdd :: Parser MathExpr -> Parser MathExpr -> Parser MathExpr
pAdd t1 e2 = try $ Add <$> t1 <*> (char '+' *> e2)

pSub :: Parser MathExpr -> Parser MathExpr -> Parser MathExpr
pSub t1 e2 = try $ Add <$> t1 <*> (Neg <$> (char '-' *> e2))

pMul :: Parser MathExpr -> Parser MathExpr -> Parser MathExpr
pMul p1 t2 = try $ Mul <$> p1 <*> (char '*' *> t2)

pDiv :: Parser MathExpr -> Parser MathExpr -> Parser MathExpr
pDiv p1 t2 = try $ Mul <$> p1 <*> (Recip <$> (char '/' *> t2))

pPowerOf :: Parser MathExpr -> Parser MathExpr -> Parser MathExpr
pPowerOf b1 p2 = try $ Exp <$> b1 <*> (char '^' *> p2)

pNumber :: Parser MathExpr
pNumber = N <$> (try pFloat <|> try pInt <?> "number")

pInt :: Parser Float
pInt = try $ read <$> many1 digit

pFloat :: Parser Float
pFloat = try $ read <$> do
    whole <- many1 digit
    point <- string "."
    decimal <- many1 digit
    return $ whole ++ point ++ decimal

pVar :: Parser MathExpr
pVar = try $  Var . (:[]) <$> letter


doParse :: String -> Either ParseError MathExpr
doParse = parse (pMathExpr <* eof) ""

-- >>> parseTest pMathExpr "3"
-- No instance for (Show (String -> IO ()))
--   arising from a use of ‘evalPrint’
--   (maybe you haven't applied a function to enough arguments?)
