module Parser (
    Parser(..), Environment(..), Functor(..), Applicative(..), Monad(..), Alternative(..),
    updateEnvironment, updateArray, modifyEnvironment, modifyArray,
    readArrayVariable, readSingleVariable, writeArray, searchVariable,
    parse, item, sat
) where

import Variable
import Control.Applicative

type Environment = [Variable] -- Environment is a list of variables

updateEnvironment :: Variable -> Parser String
updateArray :: Variable -> Int -> Int -> Parser String
modifyEnvironment :: Environment -> Variable -> Environment
modifyArray :: Environment -> Variable -> Int -> Int -> Environment
readArrayVariable :: Identifier -> DataType -> Int -> Parser Int
readSingleVariable :: Identifier -> DataType -> Parser Int
writeArray  :: Identifier -> Int -> Int -> Parser [Int]
searchVariable :: Environment -> Identifier -> DataType -> [Value Int]

updateEnvironment var = P (\env inp -> case inp of
	xs -> [((modifyEnvironment env var), "", xs)])
		
updateArray var index value = P (\env inp -> case inp of
	xs -> [((modifyArray env var index value), "", xs)])
	
modifyArray [] var i v = [writeVariable var (i, v)]
modifyArray (x:xs) var i v = if (x == var) then 
	[writeVariable x (i, v)] ++ xs
	else [x] ++ modifyArray xs var i v
	
modifyEnvironment [] var = [var]
modifyEnvironment (x:xs) var = if (x == var) then
	[var] ++ xs
	else [x] ++ modifyEnvironment xs var

-- Search a variables stored in memory
searchVariable [] _ _ = []
searchVariable (x:xs) n t =  if ((n == (name x)) && (t == (typeV x)))
	then [value x]
	else searchVariable xs n t
		
writeArray i n v = P (\env inp -> case searchVariable env i ArrayInt of 
	[] -> []
	[Val x] -> []
	[Array xs] -> [(env, replace xs (n, v), inp)])

-- Returns the name of a variable given the type and the name 
readArrayIntVariable n t = P (\env inp -> case searchVariable env n t of
	[] -> []
	[Val x] -> []
	[Array xs] -> [(env, xs, inp)])

readSingleVariable n t = P (\env inp -> case searchVariable env n t of
	[] -> []
	[Val v] -> [(env, v, inp)]
	[Array xs] -> [])

readArrayVariable n t i = P(\env inp -> case searchVariable env n t of
	[] -> []
	[Val v] -> []
	[Array xs] -> [(env, (xs !! i), inp)])

newtype Parser a = P (Environment -> String -> [(Environment, a, String)]) -- Allows Parser to be made into instances of classes

parse :: Parser a -> Environment -> String -> [(Environment, a, String)]
item :: Parser Char

parse (P p) env inp = p env inp

item = P (\env inp -> case inp of 
	[] -> []
	(x:xs) -> [(env, x, xs)])
		
instance Functor Parser where
	-- fmap :: (a -> b) -> Parser a -> Parser b
	
 	fmap g p = P (\env inp -> case parse p env inp of
  		[] -> []
	  	[(env,v,out)] -> [(env,g v, out)])

instance Applicative Parser where 
	-- pure :: a -> Parser a
	pure v = P (\env inp -> [(env, v, inp)])

	-- <*> :: Parser (a -> b) -> Parser a -> Parser b 
	pg <*> px = P (\env inp ->  case parse pg env inp of
		[] -> []
		[(env, g, out)] -> parse (fmap g px) env out)

instance Monad Parser where
	-- return :: a -> Parser a
	return = pure
		
	-- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
	p >>= f = P (\env inp -> case parse p env inp of 
		[] -> []
		[(env, v, out)] -> parse (f v) env out)

instance Alternative Parser where
	-- empty :: Parser a
	empty = P (\env inp -> [])

	-- (<|>) :: Parser a -> Parser a -> Parser a
	p <|> q = P (\env inp -> case parse p env inp of 
		[] -> parse q env inp
		[(env, v, out)] -> [(env, v, out)])

sat :: (Char -> Bool) -> Parser Char

sat p = do {
	x <- item;
	if p x then return x else empty;
}
