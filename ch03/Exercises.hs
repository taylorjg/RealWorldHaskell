import Data.List(sortBy)

-- **********************************************************************

-- Pages 69-70

-- **********************************************************************

-- Exercises 1 & 2

listLength :: [a] -> Int
listLength (x:xs) = 1 + listLength(xs)
listLength [] = 0

l1 = listLength []
l2 = listLength [1]
l3 = listLength [1,2,3,4]
l4 = listLength $ take 10 [1..]

-- **********************************************************************

-- Exercise 3

listMean :: Fractional a => [a] -> a
listMean xs = sum xs / fromIntegral (listLength xs)

-- **********************************************************************

-- Exercise 4

palindrome :: [a] -> [a]
palindrome xs = xs ++ reverse xs

-- **********************************************************************

-- Exercise 5

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = (x == last xs) && isPalindrome (init xs)

-- **********************************************************************

-- Exercise 6

sortListsByLength :: [[a]] -> [[a]]
sortListsByLength = sortBy compareListLengths
	where compareListLengths xs ys = length xs `compare` length ys

-- **********************************************************************
