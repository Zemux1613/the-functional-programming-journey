module Solution10 where

-- ===== Assignment 10.2. =====

pyths :: Int -> [(Int, Int, Int)]
pyths lim =
  [ (a, b, c) | a <- [1..lim], b <- [1..lim], c <- [max a b..lim], a * a + b * b == c * c ]

perfects :: Int -> [Int]
perfects = filter isPerfect . enumFromTo 1
  where
    isPerfect = (==) <*> sum . factors
    factors n = [ x | x <- [1..n - 1], n `mod` x == 0]

scalarprod :: [Int] -> [Int] -> Int
scalarprod a b = sum $ zipWith (*) a b

-- ===== Assignment 10.3. =====

-- This really isn't a good use-case for a list comprehension, but the task demands it :/
fibs :: [Int]
fibs = 0:1:[ a + b | (a, b) <- zip fibs $ drop 1 fibs ]

-- Functions for task 10.3.b)
nfibs :: Int -> [Int]
nfibs = flip take fibs

fibsUpTo :: Int -> [Int]
fibsUpTo = flip takeWhile fibs . (>=)

-- Task c)
-- Definitions we'll use:
--   take 0 _ = []
--   take n (x:xs) = x:(take n-1 xs)
--
--   drop 0 xs = xs
--   drop _ [] = []
--   drop n (_:xs) = drop n-1 xs
--
--   zip [] _ = []
--   zip _ [] = []
--   zip (a:as) (b:bs) = (a, b):zip as bs
--
-- Long terms that will never be evaluated further have been ellipsized for brevity.
--
--   take 3 fibs
-- = take 3 0:1:[ a + b | (a, b) <- zip fibs $ drop 1 fibs ]
-- = 0:(take 3-1 1:[ a + b | (a, b) <- zip fibs $ drop 1 fibs ])
-- = 0:1:(take 2-1 [ a + b | (a, b) <- zip fibs $ drop 1 fibs ])
-- = 0:1:(take 1 [ a + b | (a, b) <- zip 0:1:[...] (drop 1 0:1:[...]) ])
-- = 0:1:(take 1 [ a + b | (a, b) <- zip 0:1:[...] (drop 0 1:[...]) ])
-- = 0:1:(take 1 [ a + b | (a, b) <- zip 0:1:[...] 1:[...] ])
-- = 0:1:(take 1 [ a + b | (a, b) <- (0, 1):zip 1:[...] [...] ])
-- = 0:1:(take 1 (0 + 1):[ a + b | (a, b) <- zip 1:[...] [...] ])
-- = 0:1:(0 + 1):(take 1-1 [...])
-- = 0:1:1:(take 0 [ a + b | (a, b) <- zip 1:[...] [...] ])
-- = 0:1:1:[]
-- = [0, 1, 1]
