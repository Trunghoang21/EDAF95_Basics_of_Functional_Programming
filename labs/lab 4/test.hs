-- Laboration 4: Solve the Sudoku board. 

-- Preparatory exercises: 

-- given definitions

type BoardProb = [(String, [Int])] -- contains all the potential values for a square. 

allDigits :: [Int]
allDigits = [1,2,3,4,5,6,7,8,9]

infAllDigits = repeat allDigits

emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char ) -> BoardProb -> Maybe BoardProb
parseSquare (s, x) values
    | x == '.' || x == '0' = return values
    |isDigit x = assign (digitToInt x) s values
    | otherwise = fail "not a valid grid"

parseBoard :: String -> Maybe BoardProb
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares 
{-
    type BoardProb is a type synonym (not a new type)
    Haskell use type synonyms as a alternative for creating name for existing types,
    which makes the code more readable and understandable. 

    The compiler will treat the type synonym as the orgrinal type.

    infAllDigits is an infinite list of all digits, created by using the repeat function.
    each element is allDigits. 

    the infAllDigits is used to create a empty board, where every square can potentially take any digit from 1 to 9.         
-}

-- implement helper functions. 

map2 :: (a -> c, b -> d) -> (a, b) -> (c, d)
map2 (f, g) (x, y) = (f x, g y)

{-
    map2 functions takes 2 functions and a tuple as input.
    apply the functions to the elements of the tuple and return a new tuple of results. 
-}

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f p xs = [if p x then f x else x | x <- xs]

{-
    mapIf function takes a function, a predicate and a list of elements as input. 
    for each element in the list, if the predicate is true, apply the function to the element. 
    Otherwise, behold the element as it is. 
-}

maybeOr :: Maybe a -> Maybe a -> Maybe a
maybeOr Nothing y = y
maybeOr (Just x) _ = Just x
{-
    takes two Maybe values as input. 
    returns the second value if the first is Nothing,
    otherwise returns the first value.
-}

firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x: _) = Just x
firstJust (Nothing : xs) = firstJust xs

{-
    takes a list of Maybe values as input. 
    returns the first Just value in the list, or Nothing if there are no Just values.
-}

lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList x ((y, ys): xs)
    | x == y = ys
    | otherwise = lookupList x xs
    
{-
    The function takes a comparable value and a list of tuples as input. 
    if the first element of the tuple is equal to the comparable value.
    the function returns the second element of the tuple, whic is a list of values.    
-}