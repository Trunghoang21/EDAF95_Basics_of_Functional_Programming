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
-}
flatten :: [[String]] -> [String]
flatten [] = []
flatten list = [concat sublist | sublist <- list]

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

printResult :: [String] -> IO ()
printResult puzzles = mapM_ (\puzzle -> do 
    let isVaid = verifySudoku puzzle
    printSudoku $ parseBoard puzzle
    putStrLn $ "The puzzle is " ++ (if isVaid then "valid" else "invalid") 
    )puzzles


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