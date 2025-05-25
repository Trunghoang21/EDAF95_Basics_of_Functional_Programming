-- 1. Point free notation 
module Exams.June2023 where

-- 1. Point free notation 

f x y = 3 - x / y 

g x y = [y z | z <- [1..x]]

-- need to consider here that y must be a function

-- rewrite this in point free style. 

f_n = ((3 -) .) . (/)

g_n x y = map y [1..x]

-- next step 

g_n1 x y = flip map [1..x] y

-- using flip function to flip the order of the function. 

-- g_n2 x = flip map [1..x]

-- flip the x parameter to the end 

g_n2 = flip map . flip take [1..]

-- on how to create the point free function. 

-- we could eliminate the last arguments of the function 

-- change the order of the fucntion uisng `flip` function

-- `take` to select a number of element from a list

-- ``map` used to applied function to each element of a list. 


-- 2. Type derivation. 