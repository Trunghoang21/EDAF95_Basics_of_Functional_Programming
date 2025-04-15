module Sudoku where
--import Control.Monad.RWS.Strict (MonadState(put))


-- import required function. 

-- for the 9 x 9 Sudoku board.

input_range_9 :: [Int]
input_range_9 = [1..9]

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
        square_strings = cross rows_9 cols_9
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
        rows = [cross [r] cols_9 | r <- rows_9]
        -- take 1 element from rows_4 and pair it up with every element in cols_4
        cols = [cross rows_9 [c] | c <- cols_9]
        -- take one element from cols_4 and pair it up with every element in rows_4
        rows_gr = chunksOf 3 rows_9
        cols_gr = chunksOf 3 cols_9
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
        squares = cross rows_9 cols_9    


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
        squares = cross rows_9 cols_9      


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
validSquareNumbers (sq, 0) board = (sq, reduceList input_range_9 $ lookups (getPeers sq) board)
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
        allValuesCanBePlaced = null $ reduceList input_range_9 listOfValues


validUnits :: [[String]] -> [(String, [Int])] -> Bool
--validUnits [] _ = False 
validUnits units squareWithValues =
    and [validUnit unit squareWithValues | unit <- units] 


main :: IO ()
main = do 
    putStrLn "Testing Sudoku verifier for 9 x 9 Sudoku board."
    putStrLn ""
    putStrLn ""

    -- direct conflict.
    let conflict1 = "003020600900305001001826400008102900700000008006708200002609500800203009005010300"    
    putStrLn $ "conflict1: " ++ show (verifySudoku conflict1) ++ " (Expected: False)"

    let conflict2 = "000000907000420180000705026100954000050000040000507009920108000034059000507000000"
    putStrLn $ "conflict2: " ++ show (verifySudoku conflict2) ++ " (Expected: False)"
    

    --blocking conflict. 

    let blocking1 = "005000006070009020000500107804150000000803000000092805907006100030400010200000600"
    putStrLn $ "blocking1: " ++ show (verifySudoku blocking1) ++ " (Expected: False)"

    let blocking2 = "004000200000030002390700080400009001209801307600200008010008053900040000000000800"
    putStrLn $ "blocking2: " ++ show (verifySudoku blocking2) ++ " (Expected: False)"

    -- solvable Sudoku.
    let solvable1 = "003020600900305001001806400008102900700000008006708200002609500800203009005010300"
    putStrLn $ "solvable1: " ++ show (verifySudoku solvable1) ++ " (Expected: True)"

    let solvable2 = "000000907000420180000705026100904000050000040000507009920108000034059000507000000"
    putStrLn $ "solvable2: " ++ show (verifySudoku solvable2) ++ " (Expected: True)"
     