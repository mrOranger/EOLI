module Variable(Identifier(..), DataType(..), 
    Value(..), Variable (..), writeVariable, replace
) where

type Identifier = String
data DataType = Boolean | Integer | ArrayInt | ArrayBool -- Data type for variables' values
data Value a = Val a | Array [a]

instance Eq (Value Int) where
	Val a == Val b = a == b
		
	Array [] == Array [] = True
	Array (x:xs) == Array (y:ys) = (Val x == Val y) && (Array xs == Array ys)

instance Functor Value where
	-- fmap :: (Int -> Int) -> Value Int -> Value Int
	fmap f (Val x) = Val (f x)
		
	fmap f (Array []) = Array []
	fmap f (Array xs) = Array (fmap f xs)

instance Show (Value Int) where
	show (Val a) = show a
	show (Array xs) = show xs

instance Eq DataType where
	Boolean == Boolean = True
	Integer == Integer = True
	ArrayInt == ArrayInt = True
	ArrayBool == ArrayBool = True
	_ == _ = False

instance Show DataType where 
	show Boolean = "Boolean"
	show Integer = "Integer"
	show ArrayInt = "[Integer]"
	show ArrayBool = "[Boolean]"

data Variable = Variable {
	name 	:: Identifier,
	typeV 	:: DataType,
	value 	:: Value Int
}

writeVariable :: Variable -> (Int, Int) -> Variable
writeVariable Variable {name = n, typeV = t, value = (Array [])} (_, _) = Variable {name = n, typeV = t, value = (Array [])}
writeVariable Variable {name = n, typeV = t, value = (Array xs)} (i, e) = Variable {name = n, typeV = t, value = (Array (replace xs (i, e)))}


instance Eq Variable where 
	Variable { name = n, typeV = t, value = _ } == Variable { name = n', typeV = t', value = _ } = 
		(n == n') && (t == t')

instance Show Variable where 
	show Variable { name = n, typeV = t, value = v} = n++ " :" ++ show (t) ++ " = " ++ show (v) ++ "\n"
	

replace :: [a] -> (Int, a) -> [a]
	
replace [] _ = []
replace (_:xs) (0,a) = a:xs
replace (x:xs) (n,a) =
	if n < 0
		then (x:xs)
		else x: replace xs (n-1,a)