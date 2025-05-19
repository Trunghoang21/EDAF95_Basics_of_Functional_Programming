-- ========================================================================== --
--                            EDAF95 - Assignment 2F                          --
--                   Student names: Bao Trung Hoang & Ibrahim Iskif           --
--                   Group number: 11                                         --
--                                                                            --
-- The program verifies the validity and solves a 9x9 Sudoku board.           --
-- The input file should be placed in the same directory as the program file. --
-- The program reads the file and processes the data.                         --
-- Instructions:                                                              --
-- 1. Load the file inside GHCi.                                              --
-- 2. run the main method                                                     --
-- 3. input the name of the input file                                        --
-- 4. follows the printed instructions                                        --
-- ========================================================================== --
 
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

 
squares :: [String]
squares = cross rows_9 cols_9

-- the function take a string as input and return the presentation of a sudoku board in form of [(String, Int)]
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

-- given definitions
type BoardProb = [(String, [Int])] 

-- given definitions
allDigits :: [Int]
allDigits = [1,2,3,4,5,6,7,8,9]

infAllDigits = repeat allDigits

emptyBoard = zip squares infAllDigits

parseSquare :: (String, Char ) -> BoardProb -> Maybe BoardProb
parseSquare (s, x) values
    | x == '.' || x == '0' = return values
    |isDigit x = assign (digitToInt x) s values
    | otherwise = fail "not a valid grid"

-- given function to parse the input string into a BoardProb.
parseBoard :: String -> Maybe BoardProb
parseBoard = foldr ((=<<) . parseSquare) (Just emptyBoard) . zip squares 

mapIf :: (a -> a) -> (a -> Bool) -> [a] -> [a]
mapIf _ _ [] = []
mapIf f p xs = [if p x then f x else x | x <- xs]

-- helper function used to find the first Just value in a list of Maybe values.
firstJust :: [Maybe a] -> Maybe a
firstJust [] = Nothing
firstJust (Just x: _) = Just x
firstJust (Nothing : xs) = firstJust xs

-- helper function to lookup a value in a list of tuples.
lookupList :: Eq a => a -> [(a, [b])] -> [b]
lookupList _ [] = []
lookupList x ((y, ys): xs)
    | x == y = ys
    | otherwise = lookupList x xs
    
-- Bind function for Maybe type.
maybeBind :: Maybe a -> (a -> Maybe b) -> Maybe b
maybeBind Nothing _ = Nothing
maybeBind (Just x) f = f x 

-- set Value to a square in the BoardProb 
setValue :: Int -> String -> BoardProb -> BoardProb
setValue value square board =
    mapIf transform predicate board
    where 
        transform(sq, v) = (sq, [value])  
        predicate (sq, v) = sq == square  


-- imp eliminate function.
eliminateValue :: Int -> String -> BoardProb -> BoardProb
eliminateValue value square board =
    mapIf transform predicate board
    where
        transform (sq, v) = (sq, filter (/= value) v)  
        predicate (sq, v) = sq == square 

-- eliminate a value from the list of possible values for a square
eliminate :: Int -> String -> BoardProb -> Maybe BoardProb
eliminate value square board 
    | null vals = Nothing 
    | otherwise = Just re
        where
            re = eliminateValue value square board 
            vals = lookupList square re 
            
-- assign a value to a square in the board.            
assign :: Int -> String -> BoardProb -> Maybe BoardProb
assign digit square board =
    let newBoard = setValue digit square board 
        peersList = getPeers square 
    in assign' digit peersList newBoard  

-- eliminate a value from peers of a square when the value is assigned to the square using the assign function.
assign' :: Int -> [String] -> BoardProb -> Maybe BoardProb
assign' _ [] board = Just board  
assign' val (x:xs) board = 
    eliminate val x board `maybeBind` assign' val xs   

-- solve the Sudoku board.
solveSudoku' :: [String] -> BoardProb -> Maybe BoardProb
solveSudoku' [] board = Just board -- base case: when the operation is done. 
solveSudoku' (x:xs) board = firstJust [ assign v x board `maybeBind` solveSudoku' xs | v <- lookupList x board] 

-- take a string as input and return a Maybe BoardProb.
solveSudoku :: String -> Maybe BoardProb
solveSudoku boardStr = 
    let board = parseBoard boardStr
    in solveSudoku' squares $ fromMaybe [] board


-- helper function used when processing the input file. 
-- remove the separator line from the input file.
-- The function takes a list of strings as input and returns a list of strings.
processSudoku :: [String] -> [[String]]
processSudoku [] = []
processSudoku xs = 
    let (before, after) = break (== "========") xs
        in  before : processSudoku (drop 1 after)

-- helper function used to flatten a list of strings into a single string
flatten :: [[String]] -> [String]
flatten [] = []
flatten list = [concat sublist | sublist <- list]

-- helper function used to reformat the string input
-- replace the '.' character with '0' and remove the '|' character from the string.
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


-- print the sudoku board.
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

-- helper function used to convert BoardProb into [(String, Int)].
convertToFinalBoard :: BoardProb -> [(String, Int)]
convertToFinalBoard board = 
    map (\(sq, v) -> (sq, convert v)) board
    where
        convert [v] = v
        convert _ = 0

-- helper function used to convert a board of type [(String, Int)] into a string.
fromBoardToString :: [(String, Int)] -> String
fromBoardToString board = 
    concat [show v | (_, v) <- board]
        
-- solve the current sudoku puzzle presented to the user.
solveOnePuzzle :: String -> IO ()
solveOnePuzzle current = do
    let result = solveSudoku current
    case result of
        Nothing -> do 
            putStrLn "There is no solution to this sudoku!"
            putStrLn ""
            putStrLn ""
        Just board -> do
            putStrLn "Here is the final solution:"
            putStrLn ""
            printBoard $ convertToFinalBoard board
            putStrLn ""
            putStrLn ""

-- solve all the sudoku puzzles from the given list.
solveAllPuzzles :: [String] -> IO ()
solveAllPuzzles [] = do
    putStrLn ""
    putStrLn "There are no more sudoku boards to solve."
    putStrLn "Thank you for using the program!"
    putStrLn ""
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
            putStrLn "Here is the final solution:"
            putStrLn ""
            printBoard $ convertToFinalBoard board
            putStrLn ""
            putStrLn ""
    solveAllPuzzles rest

-- helper function used when the user wants to assign a value to a square.
-- check if the sudoku board is solved or not. 
isSolved :: String -> Bool
isSolved [] = True
isSolved (x:xs)
    | x == '0' = False
    | otherwise = isSolved xs

-- helper function used to check the input from user is valid.
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

-- helper function used to update the value of a square in the board. 
updateSquareValue :: String -> Int -> [(String, Int)] -> [(String, Int)]
updateSquareValue square value board = 
    let newBoard = map (\(sq, v) -> if sq == square then (sq, value) else (sq, v)) board
    in newBoard

-- handle the user input for assigning a value to a square. 
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

-- print out the instructions for the user.
giveInstructions :: IO ()
giveInstructions = do
    putStrLn "Please choose the following options: "
    putStrLn "type s to solve this sudoku board."
    putStrLn "type a to solve all remaining sudoku boards."
    putStrLn "type i to assign a value to a square."
    putStrLn "type q to quit the program."
    putStrLn "Please enter your choice: "

-- interactive loop for the user to choose the options for solving the sudoku board. 
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
            putStrLn ""
            putStrLn "solve the current sudoku board."
            putStrLn ""
            solveOnePuzzle current
            interactiveLoop rest

        "a" -> do
            putStrLn ""
            putStrLn "solve all the remaining sudoku boards."
            putStrLn ""
            solveOnePuzzle current
            solveAllPuzzles rest

        "i" -> do
            putStrLn ""
            putStrLn "assign a value to a square."
            putStrLn ""
            userSolve (current : rest)
            --interactiveLoop rest

        _ -> do
            putStrLn ""
            putStrLn "quit the program."
            putStrLn "thank you for using the program!"
            putStrLn ""

-- main function to run the program.
main :: IO ()
main = do
    putStrLn "Please enter the file name: "
    fileName <- getLine
    contents <- readFile fileName
    let puzzles = reformat $ flatten $ processSudoku (lines contents)
    -- begins the interactive loop with the list of sudoku boards. 
    interactiveLoop puzzles
