-- Laboration 4: Solve the Sudoku board. 
import Data.Char (isDigit)
-- Define the input range for the 9x9 Sudoku board.
input_range_9 :: [Int]
input_range_9 = [1..9]

-- Define the rows and columns for the 9x9 Sudoku board.
rows_9 ="ABCDEFGHI"
cols_9 ="123456789"

cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y] | x <- xs, y <- ys]

 
-- replace '.' with '0' in the input string.
replacePointWithZeros :: String -> String 
replacePointWithZeros [] = [] 
replacePointWithZeros (x:xs) 
    | x == '.' = '0' : replacePointWithZeros xs
    | otherwise = x : replacePointWithZeros xs

 
-- takes a board string as input and returns a list of tuples.
-- Each tuple contains a square and its corresponding value.
-- value is 0 if the square is empty

{-
parseBoard :: String -> [(String, Int)]
parseBoard boardStr = 
    let convert_str = replacePointWithZeros boardStr
        values = map digitToInt convert_str
        square_strings = cross rows_9 cols_9
    in
        zip square_strings values

-} 
-- helper function to create a list of all possible squares in the Sudoku board.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)


-- unitList contains all the units in the Sudoku board.
unitList :: [[String]]
unitList = rows ++ cols ++ boxes
    where
        rows = [cross [r] cols_9 | r <- rows_9]
        cols = [cross rows_9 [c] | c <- cols_9]
        rows_gr = chunksOf 3 rows_9
        cols_gr = chunksOf 3 cols_9 
        boxes = [cross rs cs | rs <- rows_gr, cs <- cols_gr]


-- given a square, filterUnitList returns the 3 units that the square belongs to.
filterUnitList :: String -> [[String]]
filterUnitList square = filter (elem square) unitList


--maps each square to its corresponding units.
units :: [(String,[[String]])] 
units = [(sq, filterUnitList sq) | sq <- squares]
    where
        squares = cross rows_9 cols_9    


-- remove all the duplicates from a list.
removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

 
-- maps each square to its corresponding peers.
-- peers are the squares that are in the same row, column, or box as the square.
peers :: [(String, [String])]
peers = [(sq, removeDuplicates(concatMap (filter (/= sq)) (filterUnitList sq))) | sq <- squares]
    where
        squares = cross rows_9 cols_9      


-- convert a character to its corresponding integer value. 
digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt c = error ("digitToInt: '" ++ [c] ++ "' is not a digit")


-- convert a Maybe value to its corresponding value.
-- If the Maybe value is Nothing, return the default value.
fromMaybe :: a -> Maybe a -> a
fromMaybe defualtValue Nothing = defualtValue
fromMaybe _ (Just x) = x 

---- get the peers of a square.
getPeers :: String -> [String]
getPeers square = fromMaybe [] (lookup square peers)

-- filter the list of Maybe values and return a list of Just values.
-- If a value is Nothing, it is removed from the list. 
justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList (Just x:xs) = x : justifyList xs


-- find the value of a key in a list of tuples.
lookups :: Eq a => [a] -> [(a,b)] -> [b]
lookups keys pairs = justifyList [lookup key pairs | key <- keys]

squares :: [String]
squares = cross rows_9 cols_9

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

-- Part 1: Implement with bind 

maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x 

{-
    the function takes a Maybe value and a function as input. 
    If the Maybevalue is Nothing, it returns Nothing. 
    Otherwise, it applies the function to the value inside the given Maybe value. 
-}

-- Task 2: 

tryReplace :: Eq a => a -> a -> [a] -> Maybe [a]
tryReplace _ _ [] = Nothing
tryReplace y y' (x:xs)
    | x == y = Just (y':xs)
    | otherwise = fmap (x:) $ tryReplace y y' xs

{-
    The functions takes 3 input, the value to be replaced, the new value and a list. 
    The function returns Nothing if the list is empty or it can not find the value to be replaced. 
    The function returns a new list, when the value is found.

    fmap function is used to preserve the structure of the list. 
    Nothing is returned if the value is not found. 
    If just value is returned, the function adds the first element to the front of the list recursively.     
-}

-- Task 3: Write function. 

recursiveReplacement :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
recursiveReplacement [] [] zs = Just zs -- base case: when the operation is done. 
recursiveReplacement _ _ [] = Nothing -- handle when the list is emtpy 
recursiveReplacement [] _ _ = Nothing -- handle missmatch case 
recursiveReplacement _ [] _ = Nothing -- handle missmatch case
recursiveReplacement (x:xs) (y:ys) zs =
    let newList = tryReplace x y zs -- maybeBind 1st arg : maybe [a]
    in case newList of 
        Nothing -> Nothing -- recursiveReplacement 3nd arg: [a]
        Just newList -> recursiveReplacement xs ys newList -- the recursive call. 

-- Part 2: The assign function. 

-- imp setValue function. 
setValue :: Int -> String -> BoardProb -> BoardProb
setValue value square board =
    mapIf transform predicate board
    where 
        transform(sq, v) = (sq, [value]) -- function: create the new tuple 
        predicate (sq, v) = sq == square -- predicate: check if the checking square is equal to the given string. 


-- imp eliminate function.
eliminateValue :: Int -> String -> BoardProb -> BoardProb
eliminateValue value square board =
    mapIf transform predicate board
    where
        transform (sq, v) = (sq, filter (/= value) v) -- function: create the new tuple, filter out the given value from the list. 
        predicate (sq, v) = sq == square -- predicate: check if the checking square is equal to the given string.


eliminate :: Int -> String -> BoardProb -> Maybe BoardProb
eliminate value square board 
    | null vals = Nothing -- check if the ressulting list is empty.
    | otherwise = Just re
        where
            re = eliminateValue value square board -- check if the square is in the board.
            vals = lookupList square re 
            
            


assign :: Int -> String -> BoardProb -> Maybe BoardProb
assign digit square board =
    let newBoard = setValue digit square board -- set the value of the square to the given digit.
        peersList = getPeers square -- get the peers of the square
    in assign' digit peersList newBoard -- call the assign' function with the new board and the peers of the square. 

assign' :: Int -> [String] -> BoardProb -> Maybe BoardProb
assign' _ [] board = Just board -- base case: when the operaetion is done. 
assign' val (x:xs) board = 
    eliminate val x board `maybeBind` assign' val xs -- the recursive call.  

-- Part 3: Sovle the Sudoku board.



main :: IO ()
main = do 
    -- Create the board string
    let board = "53..7...."
              ++ "6..195..."
              ++ ".98....6."
              ++ "8...6...3"
              ++ "4..8.3..1"
              ++ "7...2...6"
              ++ ".6....28."
              ++ "...419..5"
              ++ "....8..79"
    
    -- Print the result of parsing the board
    print $ parseBoard board



