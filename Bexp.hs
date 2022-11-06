module Bexp(
		bexp, bterm, bfactor, bcomparison,
		parseBexp, parseBterm, parseBfactor,
		parseComparison
	) where

	import Basic
	import Aexp

	bexp :: Parser Bool
	bterm :: Parser Bool
	bfactor :: Parser Bool
	bcomparison :: Parser Bool
	parseBexp :: Parser String
	parseBterm :: Parser String
	parseBfactor :: Parser String
	parseComparison :: Parser String

	bexp = do {
	 	p1 <- bterm;
	  	symbol "||";
	  	p2 <- bexp;
	  	return (p1 || p2);
	} <|> bterm

	bterm = do {
		p1 <- bfactor;
	  	symbol "&&";
	  	p2 <- bterm;
	  	return (p1 && p2);
	} <|> bfactor
	 
	bfactor = do {
		p <- rbrackets bexp;
	  	return (p);
	} <|> bcomparison <|> do {
		symbol "True";
	 	return True;
	} <|> do {
	 	symbol "False";
	 	return False;
	} <|> do{
	  	i <- identifier;
	  	v <- readSingleVariable i Boolean;
	  	if (v == 0) then return False else return True;
	} <|> do {
	  	symbol "!";
	  	p <- bfactor;
	  	return (not p);  
	} <|> do {
		i <- identifier;
		n <- brackets aexpr;
		v <- readArrayVariable i ArrayBool n;
		if (v == 0) then return False else return True;
	}

	bcomparison = do {
		a1 <- aexpr;
		symbol "<";
		a2 <- aexpr;
	 	return (a1 < a2);
	} <|> do {
		a1 <- aexpr;
		symbol "<=";
		a2 <- aexpr;
	 	return (a1 <= a2);
	} <|> do {
	  	a1 <- aexpr;
	  	symbol ">";
	  	a2 <- aexpr;
	 	return (a1 > a2);
	} <|> do {
		a1 <- aexpr;
	  	symbol ">=";
	  	a2 <- aexpr;
	 	return (a1 >= a2);
	}<|> do {
	  	a1 <- aexpr;
	  	symbol "==";
	  	a2 <- aexpr;
	  	return (a1 == a2);
	} <|> do {
	  	a1 <- aexpr;
	  	symbol "!=";
	  	a2 <- aexpr;
	  	return (not (a1 == a2));
	}

	parseBexp = do {
		p1 <- parseBterm;
	  	symbol "||";
	  	p2 <- parseBexp;
	  	return (p1 ++ "||" ++ p2);
	} <|> parseBterm

	parseBterm = do {
	  	p1 <- parseBfactor;
	  	symbol "&&";
	  	p2 <- parseBterm;
	  	return (p1 ++ "&&" ++ p2);
	} <|> parseBfactor
 
	parseBfactor = do {
		p <- rbrackets parseBexp;
	  	return ("(" ++ p ++ ")");
	} <|> do {
	  	c <- parseComparison;
	  	return c;
	} <|> do {
	  	symbol "True";
	  	return "True";
	} <|> do {
	  	symbol "False";
	  	return "False";
	} <|> do {
	  	symbol "!";
	  	p <- parseBfactor;
	  	return ("!" ++ p);  
	} <|> do {
		i <- identifier;
		do {
			n <- brackets parseAexp;
			return (i ++ "[" ++ n ++ "]");
		}<|> return i;
	}

	parseComparison = do {
		a1 <- parseAexp;
	  	symbol "<";
	  	a2 <- parseAexp;
	  	return (a1 ++ "<" ++ a2);
	} <|> do {
	 	a1 <- parseAexp;
	  	symbol "<=";
	  	a2 <- parseAexp;
	  	return (a1 ++ "<=" ++ a2);
	} <|> do {
	  	a1 <- parseAexp;
	  	symbol ">";
	  	a2 <- parseAexp;
	  	return (a1 ++ ">" ++ a2);
	} <|> do {
		a1 <- parseAexp;
	  	symbol ">=";
	  	a2 <- parseAexp;
	  	return (a1 ++ ">=" ++ a2);
	}<|> do {
	  	a1 <- parseAexp;
	  	symbol "==";
	  	a2 <- parseAexp;
	  	return (a1 ++ "==" ++ a2);
	} <|> do {
	  	a1 <- parseAexp;
	  	symbol "!=";
	  	a2 <- parseAexp;
	  	return (a1 ++ "!=" ++ a2);
	}