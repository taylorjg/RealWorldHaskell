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

-- Exercise 7

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse _ (x:[]) = x
intersperse sep (x:xs) = x ++ [sep] ++ (intersperse sep xs)

-- **********************************************************************

-- Exercise 8

data Tree a = Node a (Tree a) (Tree a)
	| Empty
	deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- **********************************************************************

-- Exercise 9

data Direction = LeftTurn | RightTurn | StraightOn deriving (Show)
type Point = (Int, Int)

-- Exercise 10

calculateTurn :: Point -> Point -> Point -> Direction
calculateTurn (x1,y1) (x2,y2) (x3,y3)
	| crossProduct > 0 = LeftTurn
	| crossProduct < 0 = RightTurn
	| otherwise = StraightOn
	where crossProduct = ((x2 - x1) * (y3 - y1)) - ((y2 - y1) * (x3 - x1))

-- Exercise 11

calculateTurns :: [Point] -> [Direction]
calculateTurns ps@(a:b:c:_) = calculateTurn a b c : calculateTurns (tail ps)
calculateTurns _ = []

-- **********************************************************************
