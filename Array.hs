module Array (
    array, arrayInit, arrayAssignment, arrayExpr,
    parseArray, parseArrayInit, parseArrayAssignment, parseArrayExpr, parseArrayConcat
)where 

import Basic
import Aexp
import Bexp

array :: Parser String
arrayInit :: Parser String
arrayAssignment :: Parser String
arrayExpr :: String -> String -> Parser String
arrayConcat :: String -> String -> Parser String

parseArray :: Parser String
parseArrayInit :: Parser String
parseArrayAssignment :: Parser String
parseArrayExpr :: Parser String
parseArrayConcat :: Parser String


merge :: [a] -> [a] -> [a]
merge []     ys = ys
merge (x:xs) ys = x : (merge xs ys)


array = do {
	arrayInit <|>
	arrayAssignment
}


arrayInit = do {
    -- <type>[] <identifier>;
	t <- typeVariable;
	emptybrackets;
	i <- identifier;
	case t of 
		"int" -> do {
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array [])}
		}
		"bool" -> do {
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array [])}
		}
} <|> do {
    -- <type>[<Aexpr>] <identifier>;
    t <- typeVariable;
	n <- brackets aexpr;
	i <- identifier;
	symbol ";";
	case t of 
		"int" -> do {
	   		updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array (replicate n 0))};
		}
		"bool" -> do {
	   		updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array (replicate n 0))}
		}
}

arrayConcat i t = do {
    -- [a,b,c,d] ++ [e,f,g,h]
    case t of 
		"int" -> do {
			symbol "[";
			x <- aexpr;
			xs <- separator aexpr;
			symbol "]";
			symbol "++";
			symbol "[";
			y <- aexpr;
			ys <- separator aexpr;
			symbol "]";
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array (merge (x:xs) (y:ys)))}
		}
		"bool" -> do {
			symbol "[";
			x <- bexp;
			xs <- separator bexp;
			symbol "]";
			symbol "++";
			symbol "[";
			y <- bexp;
			ys <- separator bexp;
			symbol "]";
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array (merge (boolToInt (x:xs)) (boolToInt (y:ys))))}
		}
} <|> do {
    -- [] ++ []
	emptybrackets;
	symbol "++";
	emptybrackets;
	symbol ";";
	case t of 
		"int" -> updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array [])}
		"bool" -> updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array [])}
} <|> do {
    if(t == "int") then do {
        -- [] ++ [a,b,c,d]
		emptybrackets;
        symbol "++";
        symbol "[";
        y <- aexpr;
        ys <- separator aexpr;
        symbol "]";
        symbol ";";
        updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array (y:ys))}
    } <|> do {
        -- [a,b,c,d] ++ []
        symbol "[";
        x <- aexpr;
        xs <- separator aexpr;
        symbol "]";
        symbol "++";
		emptybrackets;
        symbol ";";
        updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array (x:xs))}
    } else do {
        symbol "[";
        x <- bexp;
        xs <- separator bexp;
        symbol "]";
        symbol "++";
		emptybrackets;
        symbol ";";
        updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array (boolToInt (x:xs)))}
    } <|> do {
		emptybrackets;
        symbol "++";
        symbol "[";
        y <- bexp;
        ys <- separator bexp;
        symbol "]";
        symbol ";";
        updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array (boolToInt (y:ys)))}
    }
}

arrayAssignment = do {
    -- <type>[] <identifier> := <ArrayExpr>;
	t <- typeVariable;
	emptybrackets;
	i <- identifier;
	symbol ":=";
	arrayExpr i t;
} <|> do {
    -- <type>[] <identifier> := <ArrayConcat>; 
	t <- typeVariable;
	emptybrackets;
	i <- identifier;
	symbol ":=";
    arrayConcat i t;
} <|> do {
	i <- identifier;
	n <- brackets aexpr;
	symbol ":=";
    do {
		-- <identifier>[<Aexpr>] := <Aexpr>;
        a <- aexpr;
	    symbol ";";
	    updateArray Variable {name = i, typeV = ArrayInt, value = (Array (replicate n 0))} n a;
    }<|> do {
		-- <identifier>[<Aexpr>] := <Bexpr>;
	    b <- bexp;
	    symbol ";";
	    case b of 
		    False -> updateArray Variable {name = i, typeV = ArrayBool, value = (Array (replicate n 0))} n 0;
		    True -> updateArray Variable {name = i, typeV = ArrayBool, value = (Array (replicate n 0))} n 1;
    }
} <|> do {
    -- <identifier> := <ArrayConcat>;
    i <- identifier;
	symbol ":=";
    arrayConcat i "int" <|> arrayConcat i "bool"
} <|> do {
    -- <identifier> := <ArrayExpr>;
    t <- typeVariable;
	emptybrackets;
	i <- identifier;
	symbol ":=";
	arrayExpr i t;
}

arrayExpr i t = do {
	-- <ArrayExpr> ::= [];
	symbol "[";
	symbol "]";
	symbol ";";
	case t of 
		"int" -> do {
			updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array [])}
		}
		"bool" -> do {
			updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array [])}
		}
} <|> do {
	-- <ArrayExpr> ::= [<Element>];
    symbol "[";
	case t of 
		"int" -> do {
			x <- aexpr;
			xs <- separator aexpr;
			symbol "]";
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayInt, value = (Array (x:xs))}
		}
		"bool" -> do {
			x <- bexp;
			xs <- separator bexp;
			symbol "]";
			symbol ";";
			updateEnvironment Variable {name = i, typeV = ArrayBool, value = (Array (boolToInt (x:xs)))}
		}
}

parseArray = do {
	parseArrayInit <|> 
	parseArrayAssignment <|> 
	parseArrayExpr <|> 
	parseArrayConcat <|>
	parseArrayInit
}

parseArrayInit = do {
    t <- typeVariable;
	e <- emptybrackets;
	i <- identifier;
	symbol ";";
	return (t ++ e ++ i ++ ";");
} <|> do {
    t <- typeVariable;
	n <- brackets parseAexp;
	x <- identifier;
	symbol ";";
	return (t ++ "[" ++ n ++ "] " ++ x ++ ";");
}

parseArrayAssignment = do {
    x <- identifier;
	n <- brackets parseAexp;
	symbol ":=";
	do {
		a <- parseAexp;
		symbol ";";
		return (x ++ "[" ++ n ++ "] :=" ++ a ++ ";");
	}<|> do {
		b <- parseBexp;
		symbol ";";
		return (x ++ "[" ++ n ++ "] :=" ++ b ++ ";");
	}
} <|> do {
    t <- typeVariable;
	e <- emptybrackets;
	i <- identifier;
	symbol ":=";
	xs <- parseArray;
	symbol ";";
	return (t ++ e ++ i ++ " := " ++ xs ++ ";");
}

parseArrayExpr = do {
    symbol "[";
	x <- parseAexp;
	xs <- separator parseAexp;
	symbol "]";
	return ("[" ++ x ++ (filter (\x -> (x /= ' ')) (unwords ["," ++ x | x <- xs])) ++  "]");
} <|> do {
	symbol "[";
	x <- parseBexp;
	xs <- separator parseBexp;
	symbol "]";
	return ("[" ++ x ++ (filter (\x -> (x /= ' ')) (unwords ["," ++ x | x <- xs])) ++  "]");
} <|> do {
	e <- emptybrackets;
	return (e);
}

parseArrayConcat = do {
	i <- identifier;
	symbol ":=";
	xs <- parseArray;
	symbol "++";
	ys <- parseArray;
	symbol ";";
	return (i ++ " := " ++ xs ++ " ++ " ++ ys ++ ";");
} <|> do {
    t <- typeVariable;
	e <- emptybrackets;
	i <- identifier;
	symbol ":=";
	xs <- parseArray;
	symbol "++";
	ys <- parseArray;
	symbol ";";
	return (t ++ e ++ i ++ " := " ++ xs ++ " ++ " ++ ys ++ ";");
}