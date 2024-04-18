
{-
Program Name: EECS 468 Assignment 7
Brief Description: Haskell functions for parts 1 and 2
Inputs: Described individually for each
Outputs: Described individually for each
Author Name: Henry Michael Hoopes
KUID: 3037626
Creation Date: 04/07/2024
-}
---------------------------------------------------------------------------------------
--PART 1

--code for replicate
--Description: produces list of n identical items
--Inputs: n -> count of identical items, a -> item
--Outputs: list of n a's
replicate' :: Int -> a -> [a]
replicate' n a = [a | x <- [1..n]];

----------------------------------------------------------

--factors produces all the factors of an integer in a list
factors' :: Int -> [Int]
--for all values of x from 1 to n-1, adds x to list if n/x has no reminder
factors' n = [x | x <- [1..n-1], n `mod` x==0]

--Description: perfects takes in an integer and returns a list of all perfect numbers
--             up to the integer
--Inputs: integer n
--Outputs: list of all perfect numbers less than n
perfects :: Int -> [Int]
--for all values of x from 1 to n, add x to list if the sum of the factors of x is x
perfects n = [x | x <- [1..n], sum (factors' x) == x]

----------------------------------------------------------

--code for find
--Description: function that returns a list of all values that pair with 
--provided key from a list of key-value pairs
--Inputs: a key and list of key-value pairs
--Outputs: outputs a list of all value pairs
find :: Eq a => a -> [(a, b)] -> [b]
--for each pair in pairs, add b to list if a' matches given key
find a pairs = [b | (a',b) <- pairs, a == a']

----------------------------------------------------------

--code for positions
--Description: function that returns the index of each occurence of element a in list of elements
--Inputs: element a and list of elements same type as a
--Outputs: list of indices
positions :: Eq a => a -> [a] -> [Int]
--create list of pairs in format (a, index) and use find to return index of all elements with first 
--val a
positions a as = find a (zip as [x | x <- [0..(length as)]])

----------------------------------------------------------

--code for scalar product
--Description: function that takes two lists and returns sum of products xi*yi for 0<i<n-1
--Inputs: two equal length lists
--Outputs: integer value
scalarproduct :: [Int] -> [Int] -> Int
--create list of pairs using zip, and then for each pair, add product to list. return sum of that list
scalarproduct xs ys = sum [x*y | (x, y) <- zip xs ys]

---------------------------------------------------------------------------------------
--PART 2

--factorial
--function that returns factorial 
factorial :: Integer -> Integer
--factorial of 0 is 1
factorial 0 = 1
--factorial of n is n*factorial(n-1)
factorial n = n * factorial(n-1)

--Combo
--function that imlements combo 
combo :: Fractional a => Integer -> Integer -> a
--for combo (n, r), return n!/(n-r)!r!
combo n r = fromIntegral(factorial n) / (fromIntegral(factorial (n-r)) * fromIntegral(factorial r))

--------------------------------------------------------
--dis objs to dis. boxes
--Description: function that returns number of ways to distribute distinguishable objects
--into distinguishable boxes
--Inputs: number of distinguishable objects, number of boxes, size of each box
--Outputs: number of ways to distribute objects among boxes
dOdB:: Fractional a => Integer -> Integer -> Integer -> a
--implements n!/n1!*..*nk! with n1!*..*nk! = (b boxes with k elements) (n!)^b*(n-b*k)!
dOdB n k b = fromIntegral(factorial n) / (fromIntegral((factorial(k)^b)*factorial(n-b*k)))

--------------------------------------------------------
--indis objs to dis boxes
--Description: function that returns number of ways to distribute indistinguishable objects into 
--distinguihsable boxes
--Inputs: number of indis. objects, number of boxes
--Outputs: number of ways to distribute objects among boxes
iOdB :: Fractional a => Integer -> Integer -> a
--implements combo (n+k-1, n)
iOdB n k = combo (n+k-1) n

--------------------------------------------------------
--stirling
--function that finds stirling number given n and j
stir :: Fractional a => Integer -> Integer -> a
--performs sum of (-1^i*combo(j,i)*(j-i)^n) for 0<i<j-1 and divides it by j!
stir n j = sum [fromIntegral((-1)^i) * (combo j i) * fromIntegral((j-i)^n) | i <- [0..j-1]] / fromIntegral(factorial j)

--dis objs to indis boxes
--Description: function that returns number of ways to distribute distinguishable objects into 
--indistinguishable boxes
--Inputs: number of boxes and objects
--Outputs: number of ways to distribute objects
dOiB :: Fractional a => Integer -> Integer -> a
--performs sum of striling number for 1<j<k
dOiB n k = sum [stir n j | j <- [1..k]]

--------------------------------------------------------
--inidis objs to inidis boxes  
--Description: function that returns number of ways to distribute indistinguishable objects into 
--indistinguishable boxes
--Inputs: number of boxes and objects
--Outputs: number of ways to distribute
iOiB :: Integer -> Integer -> Integer
--only 1 way to distribute 0 objects into any boxes 
iOiB 0 _ = 1
--only 1 way to distribute any number of objects among 1 box
iOiB _ 1 = 1
iOiB n k 
--if we have 0 boxes or less than 0 objects, return 0
 | k < 1 || n < 0 = 0
--else, return sum of recursive call with n=n-k and recursive call with k=k-1
 | otherwise = (iOiB (n-k) k) + (iOiB n (k-1))