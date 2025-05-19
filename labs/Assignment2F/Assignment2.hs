-- Sudoku solver
-- Author: Bao Trung Hoang 
-- Instructions for the solution: 

-- Laboration 4: Solve the Sudoku board. 
import Data.Char (isDigit)
import System.IO
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

squares :: [String]
squares = cross rows_9 cols_9

parseBoard_ :: String -> [(String, Int)]
parseBoard_ boardStr = 
    zip squares $ map digitToInt boardStr

     
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

solveSudoku' :: [String] -> BoardProb -> Maybe BoardProb
solveSudoku' [] board = Just board -- base case: when the operation is done. 
solveSudoku' (x:xs) board = firstJust [ assign v x board `maybeBind` solveSudoku' xs | v <- lookupList x board] 


solveSudoku :: String -> Maybe BoardProb
solveSudoku boardStr = 
    let board = parseBoard boardStr
    in solveSudoku' squares $ fromMaybe [] board



-- Read the input file. 


processSudoku :: [String] -> [[String]]
processSudoku [] = []
processSudoku xs = 
    let (before, after) = break (== "========") xs
        in  before : processSudoku (drop 1 after)
{-
    by changing the returned value of the function, we could remove the separator line from the result.
    from [String] -> [String] to [String] -> [[String]]
    Each element of the sublist will represent a sudoku board line.

    the break function is used to split the list into 2 parts: it takes place when the condition is met. 
    before varibale stores the first part of the list before the separator line. The after variable stores lines from the 
    separator line to the end of the list. 

    The drop function is used to remove the element at the given index from the list. 
    int -> [a] -> [a]
    The drop function is used to remove the first element, which is the separator line from the after list. 
    The resulting list is passed to the processSudoku function recursively.

    : operator is used to append the element to the front of the list. 
    base case of the recurssion is when the input list is empty. The element is added in the revserse order.   
-}
flatten :: [[String]] -> [String]
flatten [] = []
flatten list = [concat sublist | sublist <- list]

{- 
    flatten functions takes a list of sublists. Each sublist is a sudoku board containing 9 lines. 
    concat function is used to join the elements of the sublist into a single string. 
    concat: [[a]] -> [a]
-}

reformat :: [String] -> [String]
reformat [] = []
reformat (string : strings)  = replaceString string : reformat strings
    where 
        replaceString :: String -> String
        replaceString [] = []
        replaceString (x:xs)
            | x == '.' = '0' : replaceString xs
            | x == '|' =  replaceString xs
            | otherwise = x : replaceString xs

{-
    refomat function takes a list of strings and reformats each string. 
    The function replace the '.' character with '0' and removes the '|' character from the string.

    the local function replaceStrig is defined inside of the reformat function. 
    The function is applied to each element of the list. 
-}

printBoard :: [(String, Int)] -> IO()
printBoard board = do
    putStrLn "   1  2  3  4  5  6  7  8  9"
    putStrLn " +---------------------------+"
    -- Create and print for each row. 
    mapM_ (\r -> do
        --let first = True
        putStr $ [r] ++ "|"
        -- Create and print for each column.
        mapM_(\c -> do
            let sq = [r,c] 
            let val = fromJust $ lookup sq board
            let displayVal = if val == 0 then "." else (show val)
            putStr $  " " ++ displayVal ++ " "
            ) cols_9
        putStrLn "|"
        ) rows_9
    putStrLn " +---------------------------+"
    where
        fromJust :: Maybe Int -> Int
        fromJust (Just x) = x
        fromJust Nothing = 0

convertToFinalBoard :: BoardProb -> [(String, Int)]
convertToFinalBoard board = 
    map (\(sq, v) -> (sq, convert v)) board
    where
        convert [v] = v
        convert _ = 0

fromBoardToString :: [(String, Int)] -> String
fromBoardToString board = 
    concat [show v | (_, v) <- board]

-- not used function : to be deleted. 
printSolution :: Maybe BoardProb -> IO ()
printSolution Nothing = putStrLn "There are no solution to this sudoku!"
printSolution (Just board) = do
    putStrLn "Here is the solution: "
    putStrLn "   1  2  3  4  5  6  7  8  9"
    putStrLn " +---------------------------+"
    -- Create and print for each row. 
    mapM_ (\r -> do
        --let first = True
        putStr $ [r] ++ "|"
        -- Create and print for each column.
        mapM_(\c -> do

            let sq = [r,c] 
            let val = fromJust $ lookup sq board
            let displayVal = if val == 0 then "." else (show val)
            putStr $  " " ++ displayVal ++ " "
            ) cols_9
        putStrLn "|"
        ) rows_9
    putStrLn " +---------------------------+"
    where
        fromJust :: Maybe [Int] -> Int
        fromJust (Just [x]) = x
        fromJust Nothing = 0 
        
-- not used function : to be deleted.
printResult :: [String] -> IO ()
printResult puzzles = mapM_ (\puzzle -> do 
    printBoard $ parseBoard_ puzzle
    putStrLn ""
    putStrLn ""
    let result = solveSudoku puzzle
    case result of 
        Nothing -> putStrLn "There is no solution to this sudoku!"
        Just board -> do
            printBoard $ convertToFinalBoard board
            putStrLn ""
            putStrLn ""
    ) puzzles

solveOnePuzzle :: String -> IO ()
solveOnePuzzle current = do
    let result = solveSudoku current
    case result of
        Nothing -> do 
            putStrLn "There is no solution to this sudoku!"
            putStrLn ""
            putStrLn ""
        Just board -> do
            printBoard $ convertToFinalBoard board
            putStrLn ""
            putStrLn ""

solveAllPuzzles :: [String] -> IO ()
solveAllPuzzles [] = return ()
solveAllPuzzles (current : rest) = do
    putStrLn "The current sudoku board: "
    printBoard $ parseBoard_ current
    putStrLn ""
    putStrLn ""
    let result = solveSudoku current
    case result of
        Nothing -> do 
            putStrLn "There is no solution to this sudoku!"
            putStrLn ""
            putStrLn ""
        Just board -> do
            printBoard $ convertToFinalBoard board
            putStrLn ""
            putStrLn ""
    solveAllPuzzles rest

-- helper function to check if all the squares are filled with values. 
isSolved :: String -> Bool
isSolved [] = True
isSolved (x:xs)
    | x == '0' = False
    | otherwise = isSolved xs

-- helper function to check the input of the user. 
checkInput :: String -> String -> Bool
checkInput square value = 
    let validSquare = square `elem` squares
        validValue = validIntValue value  
    in validSquare && validValue
    where
        validIntValue :: String -> Bool
        validIntValue value = 
            case reads value of 
                [(v, "")] -> v `elem` input_range_9
                _ -> False

-- function for updating the sudoku board with the user input. 
updateSquareValue :: String -> Int -> [(String, Int)] -> [(String, Int)]
updateSquareValue square value board = 
    let newBoard = map (\(sq, v) -> if sq == square then (sq, value) else (sq, v)) board
    in newBoard

-- ask the user for inputs.
userSolve :: [String] -> IO ()
userSolve [] = return ()
userSolve (current:rest) = do
    putStrLn "The square you want to assign a value to: "
    square <- getLine
    putStrLn "The value you want to assign: "
    value <- getLine
    let isValidInputs = checkInput square value
    case isValidInputs of
        False -> do
            putStrLn "The input is not valid. Please try again."
            putStrLn""
            userSolve (current:rest)
        True -> do
            putStrLn "The input is valid."
            putStrLn""
            let result = parseBoard current `maybeBind` assign (read value) square
            case result of
                Nothing -> do 
                    putStrLn "There is no solution to this sudoku!"
                    putStrLn "The next sudoku board will be presented"
                    putStrLn ""
                    interactiveLoop rest
                Just board -> do
                    let newBoardStr = fromBoardToString $ updateSquareValue square (read value) (parseBoard_ current)
                    let isSolvedBoard = isSolved newBoardStr
                    case isSolvedBoard of
                        True -> do
                            putStrLn "The sudoku board is solved!"
                            putStrLn "Here is the final solution:"
                            printBoard $ convertToFinalBoard board
                            putStrLn ""
                            interactiveLoop rest
                        False -> do
                            putStrLn "The sudoku board is not solved yet."
                            interactiveLoop (newBoardStr: rest)


giveInstructions :: IO ()
giveInstructions = do
    putStrLn "Please choose the following options: "
    putStrLn "type s to solve this sudoku board."
    putStrLn "type a to solve all remaining sudoku boards."
    putStrLn "type i to assign a value to a square."
    putStrLn "type q to quit the program."
    putStrLn "Please enter your choice: "

interactiveLoop ::[String] -> IO ()
interactiveLoop [] = do
    putStrLn "There are no more sudoku boards to solve."
    putStrLn "Thank you for using the program!"
interactiveLoop (current : rest) = do
    putStrLn "Here is the current sudoku board: "
    printBoard $ parseBoard_ current
    putStrLn ""
    putStrLn ""
    giveInstructions
    choice <- getLine
    case choice of
        "s" -> do
            putStrLn "solve the current sudoku board."
            solveOnePuzzle current
            interactiveLoop rest

        "a" -> do
            putStrLn "solve all the remaining sudoku boards."
            solveOnePuzzle current
            solveAllPuzzles rest

        "i" -> do
            putStrLn "assign a value to a square."
            userSolve (current : rest)
            --interactiveLoop rest

        _ -> do
            putStrLn "quit the program."


main :: IO ()
main = do
    putStrLn "Please enter the file name: "
    fileName <- getLine
    contents <- readFile fileName
    let puzzles = reformat $ flatten $ processSudoku (lines contents)

    interactiveLoop puzzles
