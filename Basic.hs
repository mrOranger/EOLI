module Basic (
		Environment (..), Parser (..), Variable(..), DataType(..), Value (..), Alternative(..),
		parse, item,
		identifier, natural, integer, symbol, typeVariable, brackets, rbrackets, emptybrackets, separator, replace, comment, multiline,
		updateEnvironment, modifyEnvironment, searchVariable, readSingleVariable, writeArray, updateArray, readArrayVariable,
		boolToInt
		
	) where

	import Data.Char
	import Variable
	import Parser

	digit 		:: Parser Char
	lower 		:: Parser Char
	upper 		:: Parser Char
	letter 		:: Parser Char
	alphanum 	:: Parser Char
	char 		:: Char -> Parser Char

	digit 		= sat isDigit
	lower 		= sat isLower
	upper 		= sat isUpper
	letter 		= sat isAlpha
	alphanum 	= sat isAlphaNum
	char x 		= sat (== x)

	string 		:: String -> Parser String
	ident 		:: Parser String
	varType		:: Parser String
	nat 		:: Parser Int
	space 		:: Parser ()
	int 		:: Parser Int
	token 		:: Parser a -> Parser a

	string [] = return []
	string (x:xs) = do {
		char x;
		string xs;
		return (x:xs);
	}

	ident = do {
		x <- lower;
		xs <- many alphanum;
		return (x:xs);
	}

	varType = do {
		string "bool";
		return "bool";
	} <|> do {
		string "int";
		return "int";
	}

	nat = do {
		xs <- some digit;
		return (read xs);
	}

	space = do {
		many (sat isSpace);
		return ();
	}

	int = do {
		char '-';
		n <- nat;
		return (-n);
	}<|> nat;

	-- Ignores the space before a token and applying a parser for a token 
	token p = do {
		space;
		v <- p;
		space;
		return v;
	}

	identifier	:: Parser String
	natural 	:: Parser Int
	integer		:: Parser Int
	symbol		:: String -> Parser String
	emptybrackets :: Parser String
	brackets 	:: Parser a -> Parser a
	rbrackets   :: Parser a -> Parser a
	typeVariable :: Parser String
	separator :: Parser a -> Parser [a]
	comment :: Parser ()
	multiline :: Parser ()

	identifier 	= token ident
	natural 	= token nat
	integer 	= token int
	symbol xs 	= token (string xs)
	typeVariable = token varType
	separator p = many (do symbol ","; p)
	comment = do {
		string "--";
		many (sat (/= '\n'));
		return ();
	}

	multiline = do 
		symbol "#"
		many (sat (/= '#'))
		symbol "#"
		return ()

	emptybrackets = do {
		symbol "[";
		symbol "]";
		return "[]";
	}
	
	brackets p = do {
		symbol "[";
		n <- p;
		symbol "]";
		return n;
	}

	rbrackets p = do {
		symbol "(";
		n <- p;
		symbol ")";
		return n;
	}

	boolToInt :: [Bool] -> [Int]

	boolToInt [] = []
	boolToInt (x:xs) | x == False = [0] ++ boolToInt xs
	                 | x == True = [1] ++ boolToInt xs