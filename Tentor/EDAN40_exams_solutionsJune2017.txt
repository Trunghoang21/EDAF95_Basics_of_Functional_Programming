-- EDAF40

-- 1. type derivation (1.5p)

-- a.
elem :: (Eq a) => a -> [a] -> Bool
flip :: (a -> b -> c) -> b -> a -> c
not :: Bool -> Bool
filter :: (a -> Bool) -> [a] -> [a]
toLower :: Char -> Char
map :: (a -> b) -> [a] -> [b]
words :: String -> [String]
reduce :: a -> a

prepare = reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|")

--thus
elem :: a -> [a] -> Bool
flip elem :: [a] -> a -> Bool
flip elem ".,:;*!#%&|" :: Char -> Bool
(not . flip elem ".,:;*!#%&|") :: Char -> Bool
filter (not . flip elem ".,:;*!#%&|") :: [Char] -> [Char]
filter (not . flip elem ".,:;*!#%&|") :: String -> String
map toLower :: String -> String
map toLower . filter (not . flip elem ".,:;*!#%&|") :: String -> String
words . map toLower . filter (not . flip elem ".,:;*!#%&|") :: String -> [String]
reduce . words . map toLower . filter (not . flip elem ".,:;*!#%&|") :: String -> [String]

--thus
prepare :: String -> [String]

-- b.
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map :: (a -> b) -> [a] -> [b]
words :: String -> [String]
toLower :: Char -> Char

rulesCompile :: [(String, [String])] -> e
rulesCompile = (map . map2) (words . map toLower, map words)

--thus
map toLower :: String -> String
words . map toLower :: String -> [String]
map words :: [String] -> [[String]]
(words . map toLower, map words) :: (String -> [String] , [String] -> [[String]])
map2 :: (a -> b, c -> d) -> (a, c) -> (b, d)
map :: (a -> b) -> [a] -> [b]
map . map2 :: (a -> b, c -> d) -> [(a, c)] -> [(b, d)]
(map . map2) (words . map toLower, map words) :: [(String , [String])] -> [([String] , [[String]])]

-- thus e is
[( [String] , [[String]] )]

-- c.
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
orElse :: Maybe a -> Maybe a -> Maybe a
foldr1 ::  (a -> a -> a) -> [ a ] -> a

-- thus
transformationApply ::  a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard :: ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard f :: [a] -> ([a], [a]) -> Maybe [a]
transformationApply wildcard f x ::  ([a], [a]) -> Maybe [a]
(transformationApply wildcard f x) ::  ([a], [a]) -> Maybe [a]
map (transformationApply wildcard f x) ::  [ ([a], [a]) ] -> [ Maybe [a] ]

--thus
pats :: [ ([a], [a]) ] 
(map (transformationApply wildcard f x) pats) :: [ Maybe [a] ]

foldr1 ::  (a -> a -> a) -> [ a ] -> a
foldr1 orElse :: [ Maybe b ] -> Maybe b

-- putting it all together
foldr1 orElse (map (transformationApply wildcard f x) pats) :: Maybe [a]

-- 2. fib (1p)

k :: Num a => [a]

[0,1,1,2,3,5,8,13,21,34]


-- 3. altMap (1p)

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]

altMap f g [] = []
altMap f g (x:[]) = [ f x ]
altMap f g (x:y:xs) = f x : g y : altMap f g xs

-- or

altmap f g [] = []
altmap f g (x:xs) = f x : altmap g f xs


-- 4. similScore (1p)

a. Version 1 is much faster than Version 2
b. because in version 1 the computation of simEntry i j is using the
tabularized values, while in version 2 it recursively attempts to
compute the value, without referring to earlier computations/table
positions.

-- 5. CircList (1.5p)



data CircList a = CircList [a] deriving Show
-- (deriving not necessary for getting full points)

perimeter (CircList l) = length l
currentelem (CircList l) = listToMaybe l

nextelem (CircList []) = Nothing
nextelem (CircList [a]) = Just a
nextelem (CircList (x:xs))= listToMaybe xs

previouselem (CircList []) = Nothing
previouselem (CircList [a]) = Just a
previouselem (CircList (x:xs)) = listToMaybe $ reverse xs

insert x (CircList a) = CircList $ (a++[x])

delete n (CircList l)
   | perimeter (CircList l) <= n = CircList []
   | otherwise                           = CircList $ drop n l

takefromCL n (CircList []) = []
takefromCL n (CircList l) = take n $ cycle l

-- extra points for noticing (Eq a) requirement!
equalCL (CircList xs) (CircList ys)
  | length xs /= length ys = False
  | null xs                          = True
  | otherwise                      = or $ map (xs==) (permutations ys)

permutations xs = take (length xs) (iterate rotate xs)

rotate [] = []
rotate (x:xs) = xs++[x]


-------------------------------------------------------------------
EDAN40

-- 1

instance Num MyNatural where
(+) = f1
(-) = f3
(*) = f2
negate Empty = Empty
negate _         = error "no negative naturals exist"
abs = id
signum Empty = Empty
signum _         = () :-: Empty
fromInteger n
  | n < 0        = error  "no negative naturals exist"
  | n == 0     = Empty
  | otherwise = () :-: fromInteger (n-1)

--2
-- see 4 in EDAF40 above

-- 3
-- a.
g :: (a -> a) -> (a -> Maybe (a,a))

-- b.
g f x = Just (x, f x)

-- 4
"our" fmap reverts the argument list, thus composition diagram won't commute:

-- a counterexample
let p be (+1)
let q be (*2)
let x be [1,2,3]
p . q = (+1) . (*2)
fmap (p . q) x = [7, 5, 3]
fmap (*2) x = [6, 4, 2]
fmap (+1) [6, 4, 2] = [3, 5, 7]

-- so
fmap (p . q) != (fmap p) . (fmap q)

-- 5

spark: "potential of a thread"
seq: forces evaluation its first argument up to WHNF, has value of the second argument
pseq: forces evaluation its first argument up to WHNF **before** returning value of the second argument
par: evaluates its first argument up to WHNF, possibly in parallel (creates a spark) with returning value of the second argument

-- 6

-- a
(.) . (.) :: (b -> c) -> (a -> a1 -> b) -> a -> a1 -> c

-- b
see in EDAF40 above

-- c
see in EDAF40 above

