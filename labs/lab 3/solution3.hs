-- ========================================================================== --
--                            EDAF95 - Assignment 1F                          --
--                   Student names: Bao Trung Hoang & Ibrahim Iskif           --
--                   Group number: 11                                         --
--                                                                            --
-- The program verifies the validity of a 9x9 Sudoku board.                   --
-- The txt files containing data are expected to be in the same directory as  --
-- the program. The program reads the files and processes the data.           --
-- The main function reads the follwings files:                               --
-- 1. blockings.txt                                                           --
-- 2. conflicts.txt                                                           --
-- 3. easy50.txt                                                              --
-- 4. inconsistent20.txt                                                      --
-- In order to see the results, users should load the file in GHCi and run    --
-- the main function.                                                         --
-- ========================================================================== --

module Sudoku where
import System.Random
-- Define the input range for the 9x9 Sudoku board.
input_range_9 :: [Int]
input_range_9 = [1..9]


-- Define the rows and columns for the 9x9 Sudoku board.
rows_9 ="ABCDEFGHI"
cols_9 ="123456789"


-- Cross product of rows_9 and cols_9
-- presents all the squares in the Sudoku board.
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
parseBoard :: String -> [(String, Int)]
parseBoard boardStr = 
    let convert_str = replacePointWithZeros boardStr
        values = map digitToInt convert_str
        square_strings = cross rows_9 cols_9
    in
        zip square_strings values

 
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


-- check if a squares value is not conflicting with its peers. 
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_ , 0) _ = True 
validSquare (sq, nb) board = 
    notElem nb $ lookups keys board 
    where
        keys = getPeers sq 
          

-- check if a square is valid using the validSquare function.
validBoard :: [(String, Int)] -> Bool
validBoard board = and [validSquare (sq, nb) board | (sq, nb) <- board]


-- the main fucntion that verifies the Sudoku board.
-- It checks if the board is valid and if all the units are valid.
verifySudoku :: String -> Bool
verifySudoku boardStr = valid_board && valid_units 
    where 
        valid_board =  validBoard $ parseBoard boardStr
        valid_units = validUnits unitList $ validBoardNumbers $ parseBoard boardStr


-- remove all the elements of list2 from list1.
reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] _ = []
reduceList list1 list2 = [x | x <- list1, x `notElem` list2]
 

-- For each square, returns a tuple containing the square and a list of valid possible values.
validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, 0) board = (sq, reduceList input_range_9 $ lookups (getPeers sq) board)
validSquareNumbers (sq, nb) board = 
    if validSquare (sq, nb) board
        then (sq, [nb])
        else (sq, []) 
     

--returns a list of tuples containing the square and a list of valid possible values.
validBoardNumbers :: [(String, Int)] -> [(String,[Int])]
validBoardNumbers board = [ validSquareNumbers (sq, nb) board | (sq, nb) <- board] 


-- check if a unit is valid.
validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit squareWithValues = 
    noConflictBetweenCellsWithSingleValue && allValuesCanBePlaced
    where 
        relevantList = filter (\(st, v) -> st `elem` unit) squareWithValues
        list_of_single_values = [v | (_, v) <- relevantList, length v == 1]
        noConflictBetweenCellsWithSingleValue = length list_of_single_values == length (removeDuplicates list_of_single_values)
        listOfValues = concat $ lookups unit squareWithValues
        allValuesCanBePlaced = null $ reduceList input_range_9 listOfValues


-- check if all the units are valid.
validUnits :: [[String]] -> [(String, [Int])] -> Bool
validUnits units squareWithValues =
    and [validUnit unit squareWithValues | unit <- units]


-- read a file containing strings for the Sudoku board and process it.
-- For each puzzle, the function verifies if it is valid or not.
readAndProcessFile :: FilePath -> IO ()
readAndProcessFile filepath = do
    putStrLn $ "File: " ++ filepath
    content <- readFile filepath
    let puzzles = processNumberLines content
    let results = zip [1..] (map verifySudoku puzzles)
    
    mapM_ (\(i, r) -> putStrLn $ "Puzzle #" ++ show i ++ ": " ++ 
                                (if r then "valid" else "invalid")) results
    
    let consistent = length $ filter id [v| (_,v) <- results]
    let total = length results
    putStrLn $ "Summary: " ++ show consistent ++ "/" ++ show total ++ " consistent"
    putStrLn ""
 

-- process the input string and return a list of strings.
processNumberLines :: String -> [String]
processNumberLines input = 
    let allLines = lines input                        
        groupedLines = foldr collectNumbers [[]] allLines  
        concatGroups = map concat groupedLines        
    in filter (not . null) concatGroups               
  where
    isNumberLine :: String -> Bool
    isNumberLine  = all (\c -> isDigit c || c == '|' || c == ' ') 
    
    isDigit :: Char -> Bool
    isDigit c = c `elem`  "0123456789"
     
    collectNumbers :: String -> [[String]] -> [[String]]
    collectNumbers line (current:rest)
        | isNumberLine line = (filter isDigit line : current) : rest
        | null current      = [] : (current : rest)
        | otherwise         = [] : (current : rest)
    collectNumbers _ []     = [[]] 

-- Laboration 3: 

-- Preparatory execises: 

-- Task 1: write the giveMeNumber that reads two numbers from the user and returns a random number betweeen them. 

-- For this task we are going to use a do block. 
giveMeNumber :: IO() = do 
    putStrLn "Input two numbers:"
    a <- getLine 
    b <- getLine
    let a' = read a :: Int -- parse the string input to an int
    let b' = read b :: Int 
    -- make a random number between a and b. 
    randomNumber <- randomRIO (a', b') :: IO Int
    putStrLn ("The random number is: " ++ show randomNumber)

-- Error handling: 
{-
    The random package is not installed by defualt. 
    For incluinng the random package, use the follwing commands:
        ghc-pkg list random # for checking if the package is installed. 
        cabal install random # for downloading the package. 
        cabal update # for updating the the local copy of the all packages index. 
        cabal install random --lib # install the random package, and make it visible to the ghci. 

    let can be used for binding of pure expressions that doesn't not invole any IO actions (side effects)
    using <- when extracting the result from an IO action, allowing to extract value from an IO context and use it as regular 
    value.
-}
-- Task 3: Print a Sudoku board

printSudoku :: [(String, Int)] -> IO()
printSudoku board = do
    putStrLn "  123456789"
    putStrLn " +-----------------+"
    -- Create and print for each row. 
    mapM_ (\r -> do
        putStr $ [r] ++ "|"
        -- Create and print for each column.
        mapM_(\c -> do
            let sq = [r,c] 
            putStr $ show $ fromJust $ lookup sq board
            ) cols_9
        putStrLn "|"
        ) rows_9
    where
        fromJust :: Maybe Int -> Int
        fromJust (Just x) = x
        fromJust Nothing = 0
         
{-
    for a square, 
        we can use the validSquare function to check for simple conflicts, conflicts between the square and its peers.
        validSquare takes a tuple (square, value) and the board as input.
        
        use the validBoardNumbers function to get all the valid values for all squares.
        use the filterUnitList function to get all 3 units that the square belongs to.
        use the validUnits function to check all the units that the square belongs to and check if they all are valid.
        validUnits takes units and the list of squares with their posible values as input.
-}

main :: IO ()
main = do 
    -- A regular Sudoku puzzle with some filled cells
    let examplePuzzle = "530070000600195000098000060800060003400803001700020006060000280000419005000080079"
    let exampleBoard = parseBoard examplePuzzle
    printSudoku exampleBoard

    
   
     