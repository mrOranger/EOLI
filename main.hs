import Basic
import Cexp
import System.IO
import System.Process
import Data.Text

main 		     :: IO String
execute 	     :: [(Environment, String, String)] -> String
getMemory 	     :: [(Environment, String, String)] -> String
menu 		     :: IO String
logo             :: IO String

-- Print the logo of the program
logo = do {
    putStrLn $ "\t Welcom in EOLI (Edoardo Oranger Language Interpreter v1.0)!";
    putStrLn $ "\t Write the code to execute, or load it from file using :load <file-name>";
    menu;
}

-- Split a string based on a character
wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case Prelude.dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = Prelude.break p s'

-- The main menu of the program
menu = do {
    putStr $ "EOLI>";
    hFlush stdout;
    input <- getLine;

    -- Checks if the user's typed :load
    if ((Prelude.head (wordsWhen (==' ') input)) == ":load") then do {
        fileContent <- readFile ((Prelude.head (Prelude.tail (wordsWhen (==' ') input))) ++ ".txt");
        putStrLn (execute (parse parseProgram [] fileContent)); -- Read the content of the file and parse it
        menu;
    }else if (input == "exit") then 
        return "Bye!"
        else do {
            putStrLn (execute (parse parseProgram [] input)); -- Parse just the input
            menu;
        }
}

-- Print the parsed string or the error
execute [] = "Invalid Input \n"

execute [(_, parsedString, "")] =
	"Parsed code: \n\n " ++ parsedString ++ "\n\n" ++
	"Memory: \n\n" ++ (getMemory (parse program [] parsedString))

execute [(_, parsedString, notParsedString)] = 
	"Parsed code: \n\n" ++ parsedString ++ "\n\n" ++
	"Memory \n\n" ++ (getMemory (parse program [] parsedString )) ++
	"Error: \n\n Unused input '" ++ notParsedString ++ "'\n"

-- Print the variables stored in the memory
getMemory [] = "Invalid input \n"
getMemory [(x:xs,parsedString, "")] = case typeV x of
	Boolean -> case value x of
		(Val 1) -> " Boolean: " ++ (name x) ++ " = True\n" ++ (getMemory [(xs,parsedString,"")])
		(Val 0) -> " Boolean: " ++ (name x) ++ " = False\n" ++ (getMemory [(xs,parsedString,"")])
	Integer -> " Integer: " ++ (name x) ++ " = " ++ (show (value x)) ++ "\n" ++ (getMemory [(xs,parsedString,"")])
	ArrayInt -> " [Int]: " ++ (name x) ++ " = " ++ (show (value x)) ++ "\n" ++ (getMemory [(xs,parsedString,"")])
	ArrayBool -> " [Bool]: " ++ (name x) ++ " = " ++ (showBoolean (value x)) ++ "\n" ++ (getMemory [(xs,parsedString,"")])

getMemory [(env,parsedString,notParsedString)] = case notParsedString of
	"" -> ""
	otherwise -> " Error (unused input '" ++ notParsedString ++ "')\n" ++ getMemory [(env,parsedString, "")]

showBoolean :: Value Int -> String
showBoolean (Array []) = "[]"
showBoolean (Array xs) = "[" ++ showSingleBoolean (Array xs) ++ "]";

showSingleBoolean :: Value Int -> String
showSingleBoolean (Array [x]) | x == 1 = "True"
                              | x == 0 = "False"
showSingleBoolean (Array (x:xs)) | x == 1 = "True, " ++ showSingleBoolean (Array xs)
                              | x == 0 = "False, " ++ showSingleBoolean (Array xs)
							  
main = logo;