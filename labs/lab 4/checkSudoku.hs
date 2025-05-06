import System.IO
import System.Environment (getArgs)
import Sudoku


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
printResult :: [String] -> IO ()
printResult puzzles = mapM_ (\puzzle -> do 
    let isVaid = verifySudoku puzzle
    printSudoku $ parseBoard puzzle
    putStrLn $ "The puzzle is " ++ (if isVaid then "valid" else "invalid") 
    )puzzles
{-
    the printResult functions takes a list of strings, loops through each string. 
    for each string the foollwing functions from the Sudoku module are applied:
        1. verifySudoku
        2. printSudoku.
        3. parseBoard. 
-}

main :: IO ()
main = do
    args <- getArgs
    content <- readFile (head args) -- This unwraps the IO string to a string
    --    read the file content and return it as a IO string.
    
    -- <- is used to bind the result of the IO action to a variable and unwraps the value from the IO monad. 
    -- In Haskell, we can't apply pure functions to the IO monad directly. 
    -- the following is required, such as: the <- notation used in the do block.
    
    let linesContent = lines content -- lines function splits the content into lines.  
    -- lines :: String -> [String]
    let split = reformat $ flatten $ processSudoku linesContent
    
    printResult split