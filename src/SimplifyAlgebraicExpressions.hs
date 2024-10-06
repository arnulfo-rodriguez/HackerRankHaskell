module SimplifyAlgebraicExpressions(simplifyMain) where


import Control.Applicative (Alternative(..), liftA2)
import Data.Char (isDigit, isSpace, isLower)
import System.IO
import Data.Maybe (fromMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State (State, runState, evalState, modify, get, lift)
import Control.Monad (when, forM_)
import Data.List (reverse)
import Text.Printf (printf)
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
data Expr = Negation Expr | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Power Expr Expr | Val Int | Variable Char | Monomial Int Char Int
  deriving (Show,Ord,Eq)

data Polynomial = EmptyPolynomial | SingeVariablePolynomial (Map Int Expr)  deriving Show

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
simplifyHelper (Variable v) = return (Monomial 1 v 1)
simplifyHelper m@(Monomial _ _ _) = return m
simplifyHelper (Power (Variable y) (Val v)) = return (Monomial 1 y v)
simplifyHelper (Sub exp1 exp2) = return (Add exp1 (Mul (Val (-1)) exp2))
simplifyHelper (Mul (Monomial c y v) (Variable x))   | x == y =  return (Monomial c y (v + 1))
simplifyHelper (Div (Monomial c y v) (Variable x))   | x == y =  return (Monomial c y (v - 1))
simplifyHelper (Mul (Monomial c y v) (Monomial c' y' v')) | y == y' = return (Monomial (c * c')  y (v + v'))
simplifyHelper (Div (Monomial c y v) (Monomial c' y' v')) | y == y' = return (Monomial (c `div` c')  y (v - v'))
simplifyHelper (Add x (Val 0)) = return x
simplifyHelper (Add (Val 0) x) = return x
simplifyHelper (Power v (Val 1)) = return v
simplifyHelper (Power _ (Val 0)) = return (Val 1)
simplifyHelper (Div x (Val 1)) = return x
simplifyHelper (Div (Val x) (Val y)) = return (Val $ x `div` y)
simplifyHelper (Div (Monomial value var exp) (Val v)) = return (Monomial (value `div` v) var exp)
simplifyHelper (Add (Val x) (Val y)) = return (Val $ x + y)
simplifyHelper (Add (Monomial value var exp) (Monomial value1 var1 exp1))   | var == var1 && exp == exp1 = return (Monomial (value + value1) var exp)
simplifyHelper (Mul (Val 1) x) = return x
simplifyHelper (Mul x (Val 1)) = return x
simplifyHelper (Mul (Val x) (Val y)) = return (Val $ x * y)
simplifyHelper (Mul (Monomial value var exp) (Val v)) = return (Monomial (value * v) var exp)
simplifyHelper (Mul (Val v) (Monomial value var exp)) = return (Monomial (value * v) var exp)
simplifyHelper (Mul (Monomial value var exp) (Monomial value1 var1 exp1)) | var == var1 = return (Monomial (value*value1) var (exp + exp1))
    
simplifyHelper (Mul (Add x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Add left right)
   
simplifyHelper (Mul z (Add x y)) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Mul x' z')
    right <- simplify (Mul y' z')
    return (Add left right)


simplifyHelper (Div (Add x y) z) = do
    x' <- simplify x
    y' <- simplify y
    z' <- simplify z
    left <- simplify (Div x' z')
    right <- simplify (Div y' z')
    return (Add left right)


simplifyHelper m@(Mul x y) = do
    x' <- simplify x
    y' <- simplify y
    return (Mul x' y')
simplifyHelper a@(Add x y) = do
    x' <- simplify x
    y' <- simplify y
    return  (Add x' y')
simplifyHelper d@(Div x y) = do
    x' <- simplify x
    y' <- simplify y
    return (Div x' y')
simplifyHelper p@(Power x y) = do
    x' <- simplify x
    y' <- simplify y
    let result = (Power x' y')
    return result

addToPoly:: Expr -> Polynomial -> Polynomial
addToPoly  (Val x) EmptyPolynomial = SingeVariablePolynomial $ Map.singleton 0 (Monomial x 'x' 0)
addToPoly  m@(Monomial _ _ degree)  EmptyPolynomial = SingeVariablePolynomial $ Map.singleton degree m
addToPoly  m@(Monomial coeff var degree) (SingeVariablePolynomial theMap) =
  SingeVariablePolynomial $ case Map.lookup degree theMap of
                              Nothing -> Map.insert degree m theMap
                              (Just (Monomial origCoeff _ _)) -> Map.insert degree (Monomial (coeff + origCoeff) var degree) theMap
addToPoly (Val coeff) (SingeVariablePolynomial theMap) =
  SingeVariablePolynomial $ case Map.lookup 0 theMap of
                              Nothing -> Map.insert 0 (Monomial coeff 'x' 0) theMap
                              (Just (Monomial origCoeff var _)) -> Map.insert 0 (Monomial (coeff + origCoeff) var 0) theMap
addToPoly (Add x y) poly = (addToPoly y (addToPoly x poly))
addToPoly x _ = error ("Expression: " ++ show x ++ " didn't match any pattern!")


printMono :: (Int -> IO ()) -> Expr -> IO ()
printMono prefixPrinter (Monomial coeff var degree) = do
  prefixPrinter(coeff)
  when (coeff /= 0) $ do
    if (abs(coeff) /= 1) || ((abs(coeff) == 1) && degree == 0)  then printf "%d" (abs coeff) else pure ()
    when (degree > 0) $ do
      printf "%c" var
      when (degree /= 1) $ printf "^%d" degree

data First = First | NotFirst deriving (Show, Eq)

printOperation :: Int -> IO ()
printOperation coeff = do
  putStr " "
  if (coeff > 0)
  then
    putStr "+"
  else
    putStr "-"
  putStr " "

prettyPrint :: Polynomial -> IO ()
prettyPrint (SingeVariablePolynomial (monomials)) = let
  prettyPrintRec [] = do
                        (putStr "")
  prettyPrintRec (f:rest) = do
    (printMono printOperation f)
    (prettyPrintRec rest)
  listExprsByAscendingKey = map snd . reverse . Map.toList
  (first:sortedMonomials) = listExprsByAscendingKey monomials
  in
    do
      (printMono (\ coeff -> (putStr (if (coeff >= 0) then "" else "-")))  first)
      (prettyPrintRec sortedMonomials)



-- simplifyHelper x = error ("Expression: " ++ show x ++ " didn't match any pattern!")
-- Example usage
simplifyMain :: IO ()
simplifyMain = do
    countStr <- getLine
    let count = read countStr :: Int
    Control.Monad.forM_ [1..count] $ \_-> do
      input <- System.IO.getLine
      let (Just originalExpr) = parse input
      let expr = evalState (simplify originalExpr) Map.empty
      let newPoly = addToPoly expr EmptyPolynomial
--      print (show originalExpr)
--      print (show expr)
--      print (show newPoly)
      prettyPrint newPoly
      printf "\n"
    
