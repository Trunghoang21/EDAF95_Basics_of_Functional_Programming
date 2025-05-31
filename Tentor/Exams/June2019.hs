-- Functional Programming June 2019. 

-- 1. Type derivation 
{-
    Give the type of the following expressions: 

    zipWith map 

    map zipWith

    map. zipWith

    type of
         
        zipWith ::(a -> b -> c)-> [a] -> [b] -> [c]

        map :: (a -> b) -> [a] -> [b]
    
    zipWith map has the type of [a -> b] -> [[a]] -> [[b]]
    
    the zipWith map takes a list of functions, applied the each function 
    on each list inside of the list argument in the possiton 2, and produce 
    a result as a list of lists. 
-}

{-
    the function: `map zipWith` function

    the type of this function is [a->b->c] -> [[a] -> [b] -> [c]]

-}

{-
    the function: `map.zipWith`

-}

-- 2. Programming. 

{-
    Write a function: 

        permutations :: [a] -> [[a]]

        given a list of arbitrary list with non-repeating elements, the function
        would produce all the permutations of this list.
 
-}

permutations :: [a] -> [[a]]
permutations [] = []
permutations xs = take  (length xs) (iterate swapFirst xs )
    where 
        swapFirst :: [a] -> [a]
        swapFirst (x:xs) = xs ++ [x]

-- not working. 

permutations2 ::Eq a => [a] -> [[a]]
permutations2 [] = []
permutations2 xs = [ x:ys | x <- xs, ys <- permutations $ filter (/=x) xs]
     
-- the logic is as follows: 
{-
    Select each elemnt from list to be the first element. 
    For the rest of the elements, swap place of the  first element to the last element
    number of time.

    In this way, we can create the permuatations of the list.  
-}

-- 3. List comprehension 

{-

-}

g :: [[Int]] -> [[Int]]
g [] = []
g xs = [tail x | x <- xs, (not.null) x, even (head x) ]

{-
    The first condition (not.null) must stand before the second condition. 
    This means when we encouter the empty list, the second condition will 
    not be evaluated. 
    
    The parenthesis containing not.null is needed for creating the composite 
    function. 
-}


-- rewrite the function using map and filter, instead of list comprehension. 

g1 :: [[Int]] -> [[Int]]
g1 [] = []
g1 xs = map tail $ filter (even.head) $ filter (not.null) xs

-- 4. What does the following function do? 

okänd xs =  foldr (++) [] (map (\y -> [y]) xs)

{-
    the map function takes a anonymous function puts each element inside of 
    the xs list in to a list, creating a list of lists [[]]
    
    The list of list will be passed to the foldr function as the third argument. 
    The foldr function used to concatenate each go each element and concatenate 
    them into one single list, containing the original element 

    Result okänd function takes list input xs and returns xs
-}

-- 5. Pattern matching 

-- Define the following function using pattern matching. 

oneOf :: Bool -> Bool -> Bool -> Bool 
oneOf a b c 
    | not (a or b ) = c 
    | not (b or c ) = a
    | not (a or c ) = d
    | otherwise = False

-- Rewrite using pattern mathching.

OneOf1 :: Bool -> Bool -> Bool -> Bool
OneOf1 False False True = True 
OneOf1 False True False = True 
OneOf1 True False False = True
OneOf1 _ _ _ = False

-- 6. Bind function. 

eliminate n [] g = Just g
eliminate n (s:ss) g = eliminate n s g >>= eliminate n ss 

{-
    a. Given the type of g is Grid, write the signature for the function. 

    eliminate :: a -> [b] -> Grid -> Maybe Grid

    b. The signature would change to 

    eliminate :: a -> [b] -> Grid -> [Grid]
    
    c. Would the second line be correct after this change? 

    No! The implemtation still works, it uses the monad for function instead.

-}