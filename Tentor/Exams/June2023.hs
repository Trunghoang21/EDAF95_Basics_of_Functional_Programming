-- 1. Point free notation 
module Exams.June2023 where

-- 1. Point free notation 

f x y = 3 - x / y 

g x y = [y z | z <- [1..x]]

-- need to consider here that y must be a function

-- rewrite this in point free style. 

f_n = ((3 -) .) . (/)

--g_n x y = map y [1..x]

-- next step 

--g_n1 x y = flip map [1..x] y

-- using flip function to flip the order of the function. 

-- g_n2 x = flip map [1..x]

-- flip the x parameter to the end 

--g_n2 = flip map . flip take [1..]

-- on how to create the point free function. 

-- we could eliminate the last arguments of the function 

-- change the order of the fucntion uisng `flip` function

-- `take` to select a number of element from a list

-- ``map` used to applied function to each element of a list. 


-- 2. Type derivation. 

-- a. find the type of the map iterate 
{-
    map      :: (a -> b) -> [a] -> [b]
    iterate          :: (a -> a) -> a -> [a]

    the type of the `map iterate` function wil return [[a]]
    therefore type of 
to check: :t map iterate
-}


-- b. Find the type of curry uncurry 
{-
    Note: currying is the process of transforming a function that takes multiple arguments 
    in a tuple as its arguments, into a function that takes just a single argument and return 
    a function which accepts further arguments, one by one. 
    
    curry ((a,b) -> c ) -> a -> b -> c 
    uncurry (a -> b -> c) -> ((a, b) -> c)

    This will result in an error. 

-}

-- c. Find the type of uncurry curry 
{-
    The type here will be (a -> b -> c) -> (a -> b -> c) 
-}


-- 3, Functions

-- redefine map f and filter f using foldr 

map_m :: (a -> b) -> [a] -> [b]
map_m f xs = foldr ((:) . f) [] xs  

-- the signature of the foldr function: (a -> b -> b) -> b -> [a] -> b
-- how the signature would match: 
-- f :: a -> c 
-- : :: c -> [c] -> [c]
-- (:) . f :: a -> [c] -> [c]

-- filter f using foldr 

filter_m :: (a->Bool) -> [a] -> [a]
filter_m f xs = foldr (\x xs -> if f x then x:xs else xs) [] xs 
-- explains how the function works here:
{-
    foldr takes one function, a value for the base case []
    and a list for the recursive back-tracking. 
    each element from xs will be applied to the function f, 
    meaning x 
    -> forward phase -> the value x will be added to the result list
    if True is the returned value when appling function f.
    -> backward phase -> when the xs is empty [], the default value 
    will be returned [], the partial function will have the second argument, 
    therefore the final result can be created.       

-}

filter_m1 :: (a -> Bool) -> [a] -> [a]
filter_m1 f xs = foldr (\ x rs -> if f x then [x]++rs else rs) [] xs
-- rewrite the function using the list concatenation ++

-- rewrite map 

map_m1 :: (a -> b) -> [a] -> [b]
map_m1 f xs = foldr (\x rs -> (f x):rs ) [] xs
-- the function is a lambda function 
-- foldr will applied the function on each element x in xs list
-- the function f would have its second argument when xs is empty and
-- empty list [] is returned. 

-- 4. Bind 
{-
    Consider the following function: 
    
    eliminate n [] g = Just g
    eliminate n (s:ss) g = eliminate n s g >>= eliminate1 n ss

    a. Given the type of g is Grid 
    write the signature for eliminate1 

    eliminate1 :: a -> [b] ->  Grid -> Maybe Grid 

    b. when chainging the base case to 
    
    eliminate1 n [] g = [g]
    eliminate n (s:ss) g = eliminate n s g >>= eliminate1 n ss

    eliminate1 :: a -> [b] -> Grid -> [Grid]

    c. This would also works, because list is also en monad. 
-}

-- 5. Data type: 

{-
    Define a type CircList (CL)

-}

data CircList a = CircList [a] deriving (Show, Eq)
-- data keyword creates new data type, define a new kind of container for the data

getInnerlist :: CircList a -> [a]
getInnerlist (CircList xs) = xs

-- im perimeter 
perimeter :: CircList a -> Int
perimeter ob = length xs 
    where xs = getInnerlist ob

-- imp currentelem 
currentelem :: CircList a -> Maybe a 
currentelem (CircList []) = Nothing
currentelem (CircList xs) = Just (head xs)

-- imp nextelem

nextelem :: CircList a -> Maybe a 
nextelem (CircList []) = Nothing
nextelem (CircList (x:xs)) = next_in_list xs 

next_in_list :: [a] -> Maybe a
next_in_list [] = Nothing
next_in_list (x:_) = Just x 

-- imp previouselem 

previouselem :: CircList a -> Maybe a
previouselem (CircList []) = Nothing
previouselem (CircList xs) = Just $ head $ reverse xs

-- imp insert 

insert :: a -> CircList a -> CircList a
insert el (CircList xs) = CircList (xs ++ [el]) 

-- imp delete 

delete :: Int -> CircList a -> CircList a 
delete nbr (CircList []) = CircList []
delete 0 l = l 
delete nbr (CircList(x:xs)) = delete (nbr-1) (CircList (xs)) 

-- imp takeFromCl 
takefromCL :: Int -> CircList a -> [a]
takefromCL _ (CircList []) = []
takefromCL nbr (CircList l) = take nbr $ cycle l 
-- cyle creates an infinite repetion.
-- cyle function uses lazy evaluation, which means that: 
{-
    it will calucate just enought. 
    when more element is needed: the intepreter computes incrementally.
-}

-- imp mvCurrentelem: move the first element to the last posiontion in the list 

mvCurrentelem :: CircList a -> CircList a
mvCurrentelem (CircList [x]) = CircList [x]
mvCurrentelem (CircList (x:xs)) = CircList $ xs ++ [x]

-- imp equalCL 
-- when comparing the elments in the list, the order matters. 
equalCL ::Eq a => CircList a -> CircList a -> Bool
equalCL (CircList l1) (CircList l2) 
    |  (length l1 ) /= (length l2) = False
    | otherwise = or $ map (== l1) (presentations l2)

-- create different presentations of l2
rotate :: [a] -> [a]
rotate [] = []
roate (x:xs) = xs ++ [x]

-- presentations func is implemented by using the `take` and `iterate` function: 
presentations :: [a] -> [[a]]
presentations l = take (length l) $ iterate roate l 
 
{-
    the iterate creates list of list for elemnt a [[a]], 
    the iterate function takes the result of previous run and applied again. 
    create a result of infinite elements

    Due to the lazy evaluation, the functions is only executed when the result of 
    the iterate function is required. 

    take function: takes number of elemnts from the result created by the 
    iterate func, which based on the number of element in the list.
-}

-- 