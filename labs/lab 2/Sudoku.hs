module Sudoku where
import Control.Monad.RWS.Strict (MonadState(put))


-- import required function. 

-- for the 4 x 4 Sudoku board.

rows_4 = "ABCD"
cols_4 = "1234"

input_range_4 :: [Int]
input_range_4 = [1..4]
-- for the 9 x 9 Sudoku board. 

rows_9 ="ABCDEFGHI"
cols_9 ="123456789"

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
filterUnitList square = filter (elem square) unitList

-- Task 3:

units :: [(String,[[String]])] 
units = [(sq, filterUnitList sq) | sq <- squares]
    where
        squares = cross rows_4 cols_4    


-- Task 5: write function removeDuplicates, take a list,
-- remove all the duplicates from that list. 

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)

-- Task 6: 
-- Calculate the value peers 

peers :: [(String, [String])]
peers = [(sq, removeDuplicates(filter (/= sq) (concat (filterUnitList sq)))) | sq <- squares]
    where
        squares = cross rows_4 cols_4      


-- Part 1: linting the code: 

-- Implement the digitToInt function. 
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
-- The function use pattern matching on each possible digit character. 


-- Part 2: The Maybe data type.

-- Task 1: 

-- running the code: :t lookup. 
-- the result: Eq a => a -> [(a, b)] -> Maybe b
-- Eq a: a type class constraint. a must be an instance of the Eq type class.
-- Takes a value of type a as its firt argument. 
-- Takes a list of typles [(a,b)] as its second argument. 
-- Returns a Maybe b type.

-- For which input, it returns Nothing?
-- The lookup function searches for a key in an association list. 
-- (a list of key-value pairs (tuples))
-- The function returns a Maybe value, that can be either Just something or Nothing. 
-- The Nothing value indicates that the key was not found in the list. 

-- Task 2:

-- write the fromMaybe function. 

-- The fromMaybe function takes a default value and a Maybe value.
-- If the Maybe value is Just something, it returns that something.
fromMaybe :: a -> Maybe a -> a
fromMaybe defualtValue Nothing = defualtValue
fromMaybe _ (Just x) = x 


-- Task 3: 
-- imp the getPeers functions.
getPeers :: String -> [String]
getPeers square = fromMaybe [] (lookup square peers)


-- Task 4: 
-- imp justifyList function. 

justifyList :: [Maybe a] -> [a]
justifyList [] = []
justifyList (Nothing:xs) = justifyList xs
justifyList (Just x:xs) = x : justifyList xs


-- Task 5: 
-- lookups function, takes a lit of input values
-- [a] first arugment -- list of keys to look up. 
-- [(a,b)] second argument -- list 
lookups :: Eq a => [a] -> [(a,b)] -> [b]
lookups keys pairs = justifyList [lookup key pairs | key <- keys]


-- Part 3 : Sudoku verifier.

-- Task 1: write the validSquare function. 
validSquare :: (String, Int) -> [(String, Int)] -> Bool
validSquare (_ , 0) _ = True -- If the value is 0, it's valid.
validSquare (sq, nb) board = 
    notElem nb $ lookups keys board 
    where
        keys = getPeers sq 
          

-- Task 2: write the validBoard function. 

validBoard :: [(String, Int)] -> Bool
validBoard board = all (== True) [validSquare (sq, nb) board | (sq, nb) <- board]

-- Task 3: write the VerifySudoku functio. 

verifySudoku :: String -> Bool
verifySudoku boardStr = valid_board && valid_units 
    where 
        valid_board =  validBoard $ parseBoard boardStr
        valid_units = validUnits unitList $ validBoardNumbers $ parseBoard boardStr


-- PART 4: blocking conflict. 

-- Task 1: write function reduceList. 
-- takes 2 lists as input, remove every elements that appear in the second list from the first list. 

reduceList :: Eq a => [a] -> [a] -> [a]
reduceList [] _ = []
reduceList list1 list2 = [x | x <- list1, x `notElem` list2]

-- Task 2: Rewrite the validQuare function. 

validSquareNumbers :: (String, Int) -> [(String, Int)] -> (String, [Int])
validSquareNumbers (sq, 0) board = (sq, reduceList input_range_4 $ lookups (getPeers sq) board)
validSquareNumbers (sq, nb) board = 
    if validSquare (sq, nb) board
        then (sq, [nb])
        else (sq, []) 
     

-- Task 3: write the validBoardNumbers
validBoardNumbers :: [(String, Int)] -> [(String,[Int])]
validBoardNumbers board = [ validSquareNumbers (sq, nb) board | (sq, nb) <- board] 


-- Task 4: write the validUnit function. 

--noConflictBetweenCellsWithSingleValue :: [(String, [Int])] -> Bool
--noConflictBetweenCellsWithSingleValue [] = True
--noConflictBetweenCellsWithSingleValue relevantList = 
--    length listOfValues == length (removeDuplicates listOfValues)
--    where
--        listOfValues = concat [v | (_, v) <- relevantList, length v == 1]
        -- The function checks if the length of the list of values is equal to the length of the list of unique values.



validUnit :: [String] -> [(String, [Int])] -> Bool
validUnit unit squareWithValues = 
    noConflictBetweenCellsWithSingleValue && allValuesCanBePlaced
    where 
        relevantList = filter (\(st, v) -> st `elem` unit) squareWithValues
        list_of_single_values = [v | (_, v) <- relevantList, length v == 1]
        noConflictBetweenCellsWithSingleValue = length list_of_single_values == length (removeDuplicates list_of_single_values)
        listOfValues = concat $ lookups unit squareWithValues
        allValuesCanBePlaced = null $ reduceList input_range_4 listOfValues


validUnits :: [[String]] -> [(String, [Int])] -> Bool
--validUnits [] _ = False 
validUnits units squareWithValues =
    and [validUnit unit squareWithValues | unit <- units] 


main :: IO ()
main = do 
    putStrLn "Testing Sudoku functions"
    
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
    let flattened_list = concat nestedList
    putStrLn $ "Flattened list: " ++ show flattened_list

    -- Task 5: remove duplicates. 

    putStrLn $ "Remove duplicates" ++ show (removeDuplicates flattened_list)


    -- Task 6: Test peer: 
    -- Task 6: Calculate peers
    
    let peers_list = peers
    putStrLn $ "Peers of A1: " ++ show (lookup "A1" peers_list)
    putStrLn $ "Number of peers for each square: " ++ show (length peers_list)

    -- Test Part 2 : Task 3

    let square = "A1"
    putStrLn $ "Peers of " ++ square ++ ": " ++ show (getPeers square)

    let square1 = "A2"
    putStrLn $ "Peers of " ++ square1 ++ ": " ++ show (getPeers square1)

    -- test input that does not exist.
    let square2 = "Z9"
    putStrLn $ "Peers of " ++ square2 ++ ": " ++ show (getPeers square2)

    -- Test justifyList function.

    let maybeList = [Just 1, Nothing, Just 2, Just 3, Nothing]
    
    putStrLn $ "Justified list: " ++ show (justifyList maybeList)

    putStrLn $ "Justified list: " ++ show (justifyList [Just "hello", Just "world"] )

    -- Test Lookup functions. 

    let pairs = peers -- list of pairs.
    let keys = ["A1", "B2", "C3", "A5"] -- list of keys to look up.
    let result = lookups keys pairs -- call the lookups function.
    putStrLn $ "Lookups result: " ++ show result
    -- The result will be a list of values associated with the keys in the pairs list.

    -- Part 3: Sudoku verifier.
    putStrLn "-------------Testing valid board functions-----------------"
    putStrLn ""
    putStrLn ""

    let valid_string_board = ".1..2..3...4...."
    putStrLn $ "Test: Valid board " ++ show (validBoard $ parseBoard valid_string_board) 

    let string_board_invalid_row = ".11.2..3...4...."
    putStrLn $ "Test: Invalid Row (Same number in the same row) " ++ show (validBoard $ parseBoard string_board_invalid_row)

    let string_board_invalid_column = ".1..2..3.1.4...."
    putStrLn $ "Test: Invalid Column (Same number in the same column) " ++ show (validBoard $ parseBoard string_board_invalid_column)

    let string_board_invalid_box = ".1..21.3...4...."
    putStrLn $ "Test: Invalid Box (Same number in the same box) " ++ show (validBoard $ parseBoard string_board_invalid_box)
    
    -- Part 4: Sudoku verifier.
    putStrLn "-------------Testing Sudoku verifier functions-----------------"
    putStrLn ""
    putStrLn ""

    let valid_board_3_4 = "3..42...1..2..4."
    let invalid_row_board_3_4 = "3..42...1..24.4."
    let invalid_col_board_3_4 = "3..42...3..2..4."
    let invalid_box_board_3_4 = "3..42...1.22..4."

    -- Test each board
    putStrLn $ "Test: Valid board validation:" ++ show (verifySudoku valid_board_3_4)
    

    putStrLn $ "Invalid row board validation result:" ++ show (verifySudoku invalid_row_board_3_4)
    

    putStrLn $ "Invalid column board validation result:" ++ show (verifySudoku invalid_col_board_3_4)
    

    putStrLn $ "Invalid box board validation result:" ++ show (verifySudoku invalid_box_board_3_4)

    -- Test reduceList function.Applicative
    putStrLn "-------------Testing reduceList functions-----------------"
    putStrLn ""
    putStrLn ""
    
    
    -- Test case 1: Basic case with numbers
    let test1 = reduceList [1,2,3,4,5] [2,4]
    putStrLn $ "reduceList [1,2,3,4,5] [2,4] = " ++ show test1
    putStrLn $ "Expected: [1,3,5], Got: " ++ show test1
    
    -- Test case 2: With Sudoku square identifiers
    let allSquares = ["A1","A2","A3","A4","B1","B2","B3","B4"]
    let peersOfA1 = ["A2","A3","A4","B1","C1","D1","B2"]
    let test2 = reduceList allSquares peersOfA1
    putStrLn $ "reduceList " ++ show allSquares ++ " " ++ show peersOfA1 ++ " = " ++ show test2
    putStrLn $ "Expected: [\"A1\",\"B3\",\"B4\"], Got: " ++ show test2
    

-- Task 2: Check valid validSquareNumbers function. 

    putStrLn "-------------Testing validSquareNumbers functions-----------------"
    putStrLn ""
    putStrLn ""

    let string_board_4_2 = "3..42...1.2...4."
    let parsed_board_4_2 = parseBoard string_board_4_2
    putStrLn $ "Valid Square Number for A1: " ++ show (validSquareNumbers ("A2", 0) parsed_board_4_2)

-- Task 3: Test the validBoardNumbers. 
    putStrLn "-------------Testing validBoardNumbers functions-----------------"
    putStrLn ""
    putStrLn ""

    let valid_board_string_4_3 = ".1..2..3...4...."
    let parsed_board_4_3 = parseBoard valid_board_string_4_3
    putStrLn $ "Valid Board Numbers: " ++ show (validBoardNumbers parsed_board_4_3)

--Task 4: validUnit function. 

-- Task 6: Check the Sudoku verifier.
    putStrLn "-------------Testing Sudoku verifier-----------------"
    putStrLn ""
    putStrLn ""

    let consistent1 =  "12..34....12..34"
    let consistent2 = "1.344..13..22143"
    let consistent3 = "1234341221434321"
    let inconsistent1 = "11.............."
    let inconsistent2 = "1...1............"
    let inconsistent3 = "1.2.1............"
    let inconsistent4 =  ".1..2..3..4.3..."
    let inconsistent5 = "1.3..24.3..1.4.2" 
    putStrLn "Testing Consistent Sudoku Boards:"
    putStrLn $ "consistent1: " ++ show (verifySudoku consistent1) ++ " (Expected: True)"
    putStrLn $ "consistent2: " ++ show (verifySudoku consistent2) ++ " (Expected: True)"
    putStrLn $ "consistent3: " ++ show (verifySudoku consistent3) ++ " (Expected: True)"
    
    putStrLn "\nTesting Inconsistent Sudoku Boards:"
    putStrLn $ "inconsistent1: " ++ show (verifySudoku inconsistent1) ++ " (Expected: False)"
    putStrLn $ "inconsistent2: " ++ show (verifySudoku inconsistent2) ++ " (Expected: False)"
    putStrLn $ "inconsistent3: " ++ show (verifySudoku inconsistent3) ++ " (Expected: False)"
    putStrLn $ "inconsistent4: " ++ show (verifySudoku inconsistent4) ++ " (Expected: False)"
    putStrLn $ "inconsistent5: " ++ show (verifySudoku inconsistent5) ++ " (Expected: False)" 