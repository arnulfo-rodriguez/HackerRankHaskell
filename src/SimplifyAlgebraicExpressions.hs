module SimplifyAlgebraicExpressions(simplifyMain) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit, isSpace, isLower)
import System.IO
import Data.Maybe (fromMaybe)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (result, rest) <- p input
    return (f result, rest)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (x, input)
  (Parser pf) <*> (Parser p) = Parser $ \input -> do
    (f, rest1) <- pf input
    (result, rest2) <- p rest1
    return (f result, rest2)

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \input -> do
    (result, rest1) <- p input
    runParser (f result) rest1

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input ->
    p1 input <|> p2 input

-- Basic parsers
satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    (x:xs) | predicate x -> Just (x, xs)
    _ -> Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

symbol :: Parser String
symbol = (:[]) <$> (satisfy (\ s -> isLower s && s >= 'a' && s <= 'z'))

spaces :: Parser String
spaces = many (satisfy isSpace)

-- Expression parser
data Expr = Add Expr Expr | Mul Expr Expr | Val Int | Variable String
  deriving (Show)

integer :: Parser Int
integer = read <$> some (satisfy isDigit)

value :: Parser Expr
value = Val <$> integer <|> parens expr

variable :: Parser Expr
variable = Variable <$> symbol

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

expr :: Parser Expr
expr = add

mul :: Parser Expr
mul = chainl1 term (Mul <$ (spaces *> char '*' <* spaces) <|> pure Mul)
  where
    term = value <|> variable <|> parens expr

add :: Parser Expr
add = chainl1 mul (Add <$ (spaces *> char '+' *> spaces))

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

parse :: String -> Maybe Expr
parse input = case runParser (spaces *> expr <* spaces) input of
  Just (result, "") -> Just result
  _ -> Nothing

-- Example usage
simplifyMain :: IO ()
simplifyMain = do
  input <- System.IO.getLine
  print $ parse input
