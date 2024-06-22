module SimplifyAlgebraicExpressions(simplifyMain) where

import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit, isSpace, isLower)
import System.IO
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, runState, evalState, modify, get)

newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }
type MemoizationMap = Map Expr Expr

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

symbol :: Parser Char
symbol = (satisfy isLower)

spaces :: Parser String
spaces = many (satisfy isSpace)

-- Expression parser
data Expr = Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Power Expr Expr | Val Int | Variable Char | Monomial Int Char Int
  deriving (Show,Ord,Eq)

integer :: Parser Int
integer = read <$> some (satisfy isDigit)

term :: Parser Expr
term = value <|> parens expr <|> variable

value :: Parser Expr
value = Val <$> integer

variable :: Parser Expr
variable = Variable <$> symbol

parens :: Parser a -> Parser a
parens p = char '(' *> spaces *> p <* spaces <* char ')'

expr :: Parser Expr
expr = add

binaryOperation :: Char -> (Expr -> Expr -> Expr) -> Parser (Expr -> Expr -> Expr)
binaryOperation character constructor = constructor <$ (spaces *> char character <* spaces)

mul :: Parser Expr
mul = chainLeftAssociative power ((binaryOperation '*' Mul) <|> (binaryOperation '/' Div)  <|> pure Mul)

add :: Parser Expr
add = chainLeftAssociative mul ((binaryOperation '+' Add) <|> (binaryOperation '-' Sub))

powerOperation :: Parser (Expr -> Expr -> Expr)
powerOperation = binaryOperation '^' Power

power :: Parser Expr
power = chainRightAssociative term powerOperation

chainLeftAssociative :: Parser a -> Parser (a -> a -> a) -> Parser a
chainLeftAssociative p op = p >>= rest
  where
    rest x = (do
      f <- op
      y <- p
      rest (f x y)) <|> return x

chainRightAssociative :: Parser a -> Parser (a -> a -> a) -> Parser a
chainRightAssociative p op = p >>= rest
  where
    rest x = (do
      f <- op
      y <- chainRightAssociative p op
      return (f x y)) <|> return x

parse :: String -> Maybe Expr
parse input = case runParser (spaces *> expr <* spaces) input of
  Just (result, "") -> Just result
  _ -> Nothing
 
simplify :: Expr -> State MemoizationMap Expr
simplify expr = do
    memo <- get
    case Map.lookup expr memo of
        Just simplified -> return simplified
        Nothing -> do
            simplified <- iterateUntilConverge expr
            modify $ Map.insert expr simplified
            return simplified

iterateUntilConverge :: Expr -> State MemoizationMap Expr
iterateUntilConverge expr = do
  simplified <- simplifyHelper expr
  if expr == simplified
    then return expr
    else simplify simplified
    
simplifyHelper :: Expr -> State MemoizationMap Expr
simplifyHelper v@(Val _) = return v
simplifyHelper v@(Variable _) = return v
simplifyHelper m@(Monomial _ _ _) = return m
simplifyHelper (Mul (Variable x) (Variable y))  | x == y =  return (Power (Variable x) (Val 2))
simplifyHelper (Mul (Variable x) (Power (Variable y) (Val v)))   | x == y = return (Power (Variable y) (Val (v + 1)))
simplifyHelper (Mul (Power (Variable y) (Val v)) (Variable x))   | x == y =  return ( (Power (Variable y) (Val (v + 1))))
simplifyHelper (Div (Power (Variable y) (Val v)) (Variable x))   | x == y =  return ( (Power (Variable y) (Val (v - 1))))
simplifyHelper (Mul (Power (Variable y) (Val v)) (Power (Variable y1) (Val v1))) | y == y1 = return ( (Power (Variable y) (Val (v + v1))))
simplifyHelper (Div (Power (Variable y) (Val v)) (Power (Variable y1) (Val v1))) | y == y1 = return ( (Power (Variable y) (Val (v - v1))))
simplifyHelper (Mul (Variable x) (Val y)) = return (Monomial y x 1)
simplifyHelper (Mul (Val y) (Variable x)) = return (Monomial y x 1)
simplifyHelper (Add (Variable x) y@(Val _)) = return (Add (Monomial 1 x 1) y)
simplifyHelper (Add x (Val 0)) = return x
simplifyHelper (Add (Val 0) x) = return x
simplifyHelper (Power v (Val 1)) = return v
simplifyHelper (Power _ (Val 0)) = return (Val 1)
simplifyHelper (Mul (Val 1) x) = return x
simplifyHelper (Mul x (Val 1)) = return x
simplifyHelper (Add (Val x) (Val y)) = return (Val $ x + y)
simplifyHelper (Mul (Val x) (Val y)) = return (Val $ x * y)
simplifyHelper (Div (Val x) (Val y)) = return (Val $ x `div` y)
simplifyHelper (Sub (Val x) (Val y)) = return (Val $ x - y)
simplifyHelper (Div (Add x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Div x' z')
    right <- simplify (Div y' z')
    return (Add left right)
    
simplifyHelper (Mul (Add x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Add left right)
    
simplifyHelper (Div (Sub x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Div x' z')
    right <- simplify (Div y' z')
    return (Sub left right)
    
simplifyHelper (Mul (Sub x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Sub left right)
    
simplifyHelper (Div z (Add x y)) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Div x' z')
    right <- simplify (Div y' z')
    return (Add left right)
    
simplifyHelper (Mul z (Add x y)) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Add left right)
    
simplifyHelper (Div z (Sub x y)) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Div x' z')
    right <- simplify (Div y' z')
    return (Sub left right)
    
simplifyHelper (Mul z (Sub x y)) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Sub left right)
simplifyHelper (Add (Monomial value var exp) (Monomial value1 var1 exp1))   | var == var1 && exp == exp1 = return (Monomial (value + value1) var exp)
simplifyHelper (Add (Monomial value var exp) (Variable var1))   | var == var1 = return (Monomial (value + 1) var exp)
simplifyHelper (Add (Variable var1) (Monomial value var exp))   | var == var1 = return (Monomial (value + 1) var exp)
simplifyHelper (Div (Monomial value var exp) (Val v)) = return (Monomial (value `div` v) var exp)
simplifyHelper (Mul (Monomial value var exp) (Monomial value1 var1 exp1)) | var == var1 = return (Monomial (value*value1) var (exp + exp1))
simplifyHelper (Add (Variable var1) (Variable var))   | var == var1 = return (Monomial 2 var 1)
simplifyHelper m@(Mul x y) = do
    x' <- simplify x
    y' <- simplify y
    return (Mul x' y')
simplifyHelper a@(Add x y) = do
    x' <- simplify x
    y' <- simplify y
    return  (Add x' y')
simplifyHelper s@(Sub x y) = do
    x' <- simplify x
    y' <- simplify y
    return (Sub x' y')
simplifyHelper d@(Div x y) = do
    x' <- simplify x
    y' <- simplify y
    return (Div x' y')
simplifyHelper p@(Power x y) = do
    x' <- simplify x
    y' <- simplify y
    let result = (Power x' y')
    return result

-- simplifyHelper x = error ("Expression: " ++ show x ++ " didn't match any pattern!")
-- Example usage
simplifyMain :: IO ()
simplifyMain = do
    input <- getLine
    case parse input of
        Just expr -> print $ evalState (simplify expr) Map.empty
        Nothing -> putStrLn "Invalid expression"
