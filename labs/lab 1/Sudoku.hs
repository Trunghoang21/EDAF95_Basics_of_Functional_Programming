module Sudoku where
-- import required function. 

import Data.Char (digitToInt)


-- for the 4 x 4 Sudoku board.

rows_4 = "ABCD"
cols_4 = "1234"

-- for the 9 x 9 Sudoku board. 

rows_9 ="ABCDEFGHI"
cols_9 ="123456789"

-- other functions

containsElem :: Eq a => a -> [a] -> Bool
containsElem _ [] = False
containsElem elem (x:xs)
    | elem == x = True
    | otherwise = containsElem elem xs


cross :: [a] -> [a] -> [[a]]
cross xs ys = [[x,y] | x <- xs, y <- ys]

-- The function take the elements from 2 list, combine into a single list.   

-- Task 1: 

replacePointWithZeros :: String -> String -- Define the signature of the function
replacePointWithZeros [] = [] -- Base case return a empty string 
replacePointWithZeros (x:xs) -- split the input into(the first character and the rest of the string)
    | x == '.' = '0' : replacePointWithZeros xs
    | otherwise = x : replacePointWithZeros xs

-- The resusive call will be made for the rest of the string. 
-- It's good practice to include the type signature of a function. 
-- it clarifies what the function expects as input and the expected output. 

-- Task 3: parseBoard function. 
-- the function takes a board string as input 

parseBoard :: String -> [(String, Int)]
parseBoard boardStr = 
    let convert_str = replacePointWithZeros boardStr
        values = map digitToInt convert_str
        square_strings = cross rows_4 cols_4
    in
        zip square_strings values

-- Part 2: Sudoku problem. 

-- Task 1: 

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs
  | length xs <= n = [xs]
  | otherwise = take n xs : chunksOf n (drop n xs)

unitList :: [[String]]
unitList = rows ++ cols ++ boxes
    where
        rows = [cross [r] cols_4 | r <- rows_4]
        -- take 1 element from rows_4 and pair it up with every element in cols_4
        cols = [cross rows_4 [c] | c <- cols_4]
        -- take one element from cols_4 and pair it up with every element in rows_4
        rows_gr = chunksOf 2 rows_4
        cols_gr = chunksOf 2 cols_4
        -- change the first argument to 3 for rows_9 and cols_9 
        boxes = [cross rs cs | rs <- rows_gr, cs <- cols_gr]

-- Task 2: Write filterUnitList 
-- return 3 units that the square belongs to. 

filterUnitList :: String -> [[String]]
filterUnitList square = filter (containsElem square) unitList

-- Task 3:

units :: [(String,[[String]])] 
units = [(sq, filterUnitList sq) | sq <- squares]
    where
        squares = cross rows_4 cols_4    


-- Task 4:   
foldList :: [[a]] -> [a]
foldList [] = []
foldList (x:xs) = x ++ foldList xs

-- Task 5: write function removeDuplicates, take a list,
-- remove all the duplicates from that list. 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Task 6: 
-- Calculate the value peers 

peers :: [(String, [String])]
peers = [(sq, removeDuplicates((filter (/= sq) (foldList (filterUnitList sq))))) | sq <- squares]
    where
        squares = cross rows_4 cols_4      


-- add the main function for testing. 


main :: IO ()
main = do 
    putStrLn("Testing Sudoku functions")
    
    -- Task 2: make a list of all possible square strings ["A1","A2",..,"D4"]
    
    let square_strings = cross rows_4 cols_4

    putStrLn $ "Square_strings are: " ++ show square_strings
    
    -- The show function is used here to convert values to strings. 

    let board_strings = ".1..2..3...4...."
    let the_board = parseBoard board_strings
    putStrLn $ "The board is" ++ show the_board

    -- Part 2: Sudoku problem.
    let test = unitList
    putStrLn $ "The units is" ++ show test 

    -- Task 2: 
    let units_square = filterUnitList "A1"
    putStrLn $ "the units for A1:" ++ show units_square 

    -- Task 3: 
    let units_tuple = units
    putStrLn $ "the units for each square" ++ show units_tuple

    -- Task 4: 
    
    let nestedList = [[1,2,3], [3,4,5], [6,7,8,9]]
    let flattened_list = foldList nestedList
    putStrLn $ "Flattened list: " ++ show flattened_list

    -- Task 5: remove duplicates. 

    putStrLn $ "Remove duplicates" ++ show (removeDuplicates flattened_list)


    -- Task 6: Test peer: 
    -- Task 6: Calculate peers
    
    let peers_list = peers
    putStrLn $ "Peers of A1: " ++ show (lookup "A1" peers_list)
    putStrLn $ "Number of peers for each square: " ++ show (length peers_list)