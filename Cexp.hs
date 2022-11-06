module Cexp (
	command, program, ifThenElse, while, repeateWhile,
	parseProgram, parseCommand, parseIfThenElse, 
	parseWhile, parseAssignment
	) where

	import Basic
	import Aexp
	import Bexp
	import Array

	command :: Parser String
	program :: Parser String
	skip :: Parser String
	initialization :: Parser String
	assignment :: Parser String
	ifThenElse :: Parser String
	while :: Parser String
	repeateWhile :: String -> Parser String
	parseProgram :: Parser String
	parseSkip :: Parser String
	parseCommand :: Parser String
	parseInitialization :: Parser String
	parseIfThenElse :: Parser String
	parseWhile :: Parser String
	parseFor :: Parser String
	parseAssignment :: Parser String
	parseComment :: Parser String

	program = do {
		command;
		program;
	} <|> command

	
	command =
		initialization <|> 
		assignment <|> 
	 	skip <|> 
	 	ifThenElse <|>
	 	while <|> 
		array

	initialization = do {
		t <- typeVariable;
		i <- identifier;
		symbol ";";
		case t of
			"int" -> updateEnvironment Variable {name = i, typeV = Integer, value = (Val 0)}
			"bool" -> updateEnvironment Variable {name = i, typeV = Boolean, value = (Val 0)}		
	}
	 
	assignment = do {
		t <- typeVariable;
		i <- identifier;
		symbol ":=";
		case t of 
			"int" -> do {
				v <- aexpr;
	   			symbol ";";
	   			updateEnvironment Variable {name = i, typeV = Integer, value = (Val v)};
			}
			"bool" -> do {
				v <- bexp;
	   			symbol ";";
	   			if (v == False) then updateEnvironment Variable {name = i, typeV = Boolean, value = (Val 0)} else updateEnvironment Variable {name = i, typeV = Boolean, value = (Val 1)};
			}
	}<|>do {
		i <- identifier;
		symbol ":=";
		do {
			v <- aexpr;
	   		symbol ";";
	   		updateEnvironment Variable {name = i, typeV = Integer, value = (Val v)};
		} <|> do {
			v <- bexp;
	   		symbol ";";
	   		if (v == False) then updateEnvironment Variable {name = i, typeV = Boolean, value = (Val 0)} else updateEnvironment Variable {name = i, typeV = Boolean, value = (Val 1)};
		}
	}

	skip = do {
		symbol "skip";
		symbol ";";
	}

	ifThenElse = do {
		symbol "if";
		b <- bexp;
	  	symbol "{";

	  	if (b) then
	   	do {
	    	program;
	    	symbol "}";
	     	do {
	      		symbol "else";
	      		symbol "{";
	      		parseProgram;
	      		symbol "}";
	      		return "";
	     	} <|>
	     return "";
	    }
	  	else do {
	    	parseProgram;
	    	symbol "}";
	    	do {
	     		symbol "else";
	     		symbol "{";
	     		program;
	     		symbol "}";
	     		return "";
	    	} <|>
	    	return "";
	   	}
	}

	while = do {
	  	w <- parseWhile;
	  	repeateWhile w;
	  	symbol "while";
	  	p <- bexp;
	  	symbol "{";
	  
	  	if (p) then do {
	    	program;
	    	symbol "}";
	    	repeateWhile w;
	    	while;
	  	} else do {
	    	parseProgram;
	    	symbol "}"; 
	    	return "";
	   	}
	}

	repeateWhile c = P(\env inp -> [(env, "", c ++ inp)])
	
	parseProgram = do {
		c <- parseCommand;
		p <- parseProgram;
		return (c ++ p);
	}<|> parseCommand

	parseCommand =  do {
		parseAssignment <|>
		parseSkip <|> 
		parseIfThenElse <|> 
		parseWhile <|> 
		parseFor <|>
		parseInitialization <|>
		parseArray <|> 
		parseComment
	} 

	parseSkip = do {
		symbol "skip";
		symbol ";";
		c <- parseCommand;
		return ("skip;" ++ c);
	}

	parseIfThenElse = do {
  		symbol "if";
  		b <- parseBexp;
  		symbol "{";
  		p1 <- parseProgram;
  		symbol "}";
  		do {
   			symbol "else";
   			symbol "{";
   			p2 <- parseProgram;
   			symbol "}";
   			return ("if" ++ b ++ "{" ++ p1 ++ "}else{" ++ p2 ++ "}");
  		} <|>
  		return ("if" ++ b ++ "{" ++ p1 ++ "}");
	}

	parseWhile = do {
  		symbol "while";
  		b <- parseBexp;
  		symbol "{";
  		p <- parseProgram;
  		symbol "}";
  		return ("while" ++ b ++ "{" ++ p ++ "}");
	} 

	parseFor = do {
		symbol "for";
		symbol "(";
		a <- parseAssignment;
		b <- parseBexp;
		symbol ";";
		i <- parseCommand;
		symbol ")";
		symbol "{";
		c <- parseProgram;
		symbol "}";
		return (a ++ " while (" ++ b ++ ") {" ++ c ++ i ++ "}");
	}

	parseAssignment = do {
		t <- typeVariable;
		x <- identifier;
		symbol ":=";
		case t of
			"int" -> do {
				a <- parseAexp;
				symbol ";";
				return (t ++ " " ++ x ++ ":=" ++ a ++ ";");
			}
			"bool" -> do {
				b <- parseBexp;
				symbol ";";
				return (t ++ " " ++ x ++ ":=" ++ b ++ ";");
			}
	}<|> do {
		x <- identifier;
		symbol ":=";
		do {
			a <- parseAexp;
			symbol ";";
			return (x ++ ":=" ++ a ++ ";");
		}<|> do {
			b <- parseBexp;
			symbol ";";
			return (x ++ ":=" ++ b ++ ";");
		}
	}

	parseInitialization = do {
		t <- typeVariable;
		i <- identifier;
		symbol ";";
		return (t ++ " " ++ i ++ ";");
	}

	parseComment = do {
		comment;
		return "";
	} <|> do {
		multiline;
		return "";
	}