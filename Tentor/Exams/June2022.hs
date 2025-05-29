


-- 1. Simple programming. 

-- write function permutations

permutations :: [a] -> [[a]]
permutations [] = []
permutations l = take (length l) (iterate rotate l)
    where 
        rotate :: [a] -> [a]
        rotate [] = []
        rotate (x:xs) = xs ++ [x]
{-
    note: 
    iterate func apply f on the given object (parameter 2)
    returns a result list of infinite elements

    the result in the list is produced when it's required. 
    
-}

-- 2. type derivation 

{-
    a. Which type has the function g defined as
    g xs = [f x | x <- xs, x > 3]
    where fn = replicate n ´+´ ? 
    
    the type of g is 

    g :: [Int] -> [String]

    xs must be of type [Int]
    when apply the f function, this will create a list of duplicated characters ´+´
    based on the int value from the list. 
    
    b. Find the type of curry curry

    curry function:  ((a,b) -> c) -> a -> b -> c
    
    curry curry would results in an error:
    
    the second curry will produced a function of type a -> b -> c 
    but the first curry function is expecting a function drived from a tuple (a, b),
    there for this would result in an ERROR. 

    c . find the type of curry . curry 
    curry . curry :: (((a, b1), b2) -> c) -> a -> b1 -> b2 -> c 
-}

-- 3. List comprehension

g :: [[Int]] -> [[Int]]
g [] = []
g xs = [tail x | x <- xs, not (null x), even (head x) ] 

-- the first example will use the list comprehension. 
-- List Comprehension

{-
    is a method to generate and process list
    allowing us to filter, map or apply conditional logic on the list element. 

    List comprehension have 3 main parts: 
    . Any number of the input lists
    . Any number of predicate (conditions)
    . A single out function

    for example: let ouputList = [<outputFunction> | <inputList>, <predicate>]
-}

-- rewrite the function using ´map´ and f filter instead. 

g1 :: [[Int]] -> [[Int]]
g1 [] = []
g1 xs = map (tail) $ filter (even . head) $ filter (not.null) xs


-- 4. Bindning 

--"Ukraine" >>= (\u -> flip (:) [] $ id u )

{-
    String is a list of characters. When combinding with monad, we are going to use 
    the list monad for each element in the list: 
    
    for instance, the anonymous function will be applied on each element 
    in the list. This function will add the character to an empty list and as it it

    the map function with the signatrue :: (a -> b) -> [a] -> [b] will return 
    a list of lists containing one single character 

    the concat function with the signature [[a]] -> [a], used to concatenate 
    element inside the lists into one single list, which results in [Char] = String. 

    So the value that is returned from the function is a string. 
-} 


-- 5. More programming

{-
    Example for an cartesian product. 
    A = {1,2,3} B = {a, b}
    A x B = {(1,a), (1,b), (2,a), (2,b), (3,a), (3,b)}
-}

data Relation a b = Relation [(a,b)] deriving Show

-- define two functions 

-- define union
union :: (Eq a, Eq b) => Relation a b -> Relation a b -> Relation a b
union r1 (Relation []) = r1
union (Relation []) r2 = r2
union (Relation r1) (Relation r2) = Relation $ r1 ++ [x | x <- r2, (notElem) x r1 ]


-- define intersection 

intersection :: (Eq a , Eq b ) => Relation a b -> Relation a b -> Relation a b
intersection (Relation []) (Relation _ ) = Relation []
intersection (Relation _) (Relation []) = Relation []
intersection (Relation r1) (Relation r2) = Relation $ [r | r <- r1, (elem) r r2]

-- define composition 

composition ::(Eq a, Eq b , Eq c) => Relation b c -> Relation a b -> Relation a c
composition (Relation r1) (Relation r2) = Relation $ [(a, c) | (b1, c) <- r1, (a, b2) <- r2, b1 == b2 ]

-- define closure
