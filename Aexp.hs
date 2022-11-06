module Aexp (
		aexpr, aterm, aatom, afactor,
		parseAexp, parseAterm, parseAatom, parseAfactor
	) where

	import Basic 
	import Data.Char
	
	aexpr :: Parser Int
	aterm :: Parser Int	
	aatom :: Parser Int
	afactor :: Parser Int 	
	parseAexp :: Parser String
	parseAterm :: Parser String
	parseAatom :: Parser String
	parseAfactor :: Parser String

	aexpr = do {
		t <- aterm;
		do {
			symbol "+";
			e <- aexpr;
			return (t + e);
		} <|> do {
			symbol "-";
			e <- aexpr;
			return (t - e);
		}
	}<|> aterm

	aterm = do {
		f <- aatom;
		do {
			symbol "*";
			t <- aterm;
			return (f * t);
		}<|> do {
			symbol "/";
			t <- aterm;
			return (div f t);
		}
	} <|> aatom

	aatom = do {
		a <- afactor;
		symbol "^";
		t <- aterm;
		return (a ^ t);

	} <|> afactor

	afactor = do{
		e <- rbrackets aexpr;
		return e;
	}<|> do {
		i <- identifier;
		readSingleVariable i Integer;
	} <|> do {
		i <- identifier;
		n <- brackets aexpr;
		v <- readArrayVariable i ArrayInt n;
		return v;
	} <|> integer 

	parseAexp = do{
			t <- parseAterm;
			symbol "+";
			e <- parseAexp;
			return (t ++ "+" ++ e);
		}
		<|> 
		do {
			t <- parseAterm;
			symbol "-";
			e <- parseAexp;
			return (t++ "-" ++ e);
		}
		<|> parseAterm


	parseAterm = do {
			f <- parseAatom;
			symbol "*";
			t <- parseAterm;
			return (f ++ "*" ++ t)
		}
		<|> 
		do {
			f <- parseAatom;
			symbol "/";
			t <- parseAterm;
			return (f ++ "/" ++ t)
		}
		<|>
		parseAatom

	parseAatom = do {
		f <- parseAfactor;
		symbol "^";
		a <- parseAatom;
		return (f ++ "^" ++ a);
	} <|> parseAfactor

	parseAfactor = do{
			e <- rbrackets parseAexp;
			return ("(" ++ e ++ ")");
		} <|> do {
			symbol "-";
			f <- parseAfactor;
			return ("-" ++ f);
		} <|> do {
			k <- integer;
			return (show k);
		} <|> do {
			i <- identifier;
			do {
				n <- brackets parseAexp;
				return (i ++ "[" ++ n ++ "]");
			} <|> return i;
		}