-- Problem 1
-- Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Can't call last on an empty list"
myLast [x] = x
myLast (_:xs) = myLast xs

-- Problem 2
-- Find the last but one element of a list.
myLastButOne :: [a] -> a
myLastButOne [] = error "Can't call last on a list with less than 2 elements"
myLastButOne [x] = error "Can't call last on a list with less than 2 elements"
myLastButOne [x,_] = x
myLastButOne (_:xs) = myLastButOne xs

-- Problem 3
-- Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k
	| len < k = error "Index out of range"
	| otherwise = xs !! (k - 1)
	where len = length xs

-- Problem 4
-- Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- Problem 5
-- Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (last xs) : (myReverse(init xs) ++ x:[])

-- Problem 6
-- Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
	| len <= 1 	= True
	| otherwise	= (head xs) == (last xs) && isPalindrome (belly xs)
	where len = length xs

belly :: [a] -> [a]
belly ls = tail (init ls)

-- Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
-- **************************************************

-- Problem 8
-- Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [a] = [a]
compress (x:xs)
	| x == head xs	= compress xs
	| otherwise		= x:compress xs

-- Problem 9
-- Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists
pack :: (Eq a) => [a] -> [[a]]

pack [] 	= [[]]
pack [a]	= [[a]]
pack xs		= packHelper [[]] xs

packHelper :: (Eq a) => [[a]] -> [a] -> [[a]]

packHelper x []				= x
packHelper [[]] (x:xs) 		= packHelper [[x]] xs
packHelper x (y:xs)
	| last (last x) == y	= packHelper (init x ++ [y:last x]) xs
	| otherwise 			= packHelper (x ++ [[y]]) xs

-- Problem 10
-- Run-length encoding of a list. Use the result of problem P09 to implement 
-- the so-called run-length encoding data compression method. Consecutive 
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = encodeHelper (pack xs)

encodeHelper :: [[a]] -> [(Int, a)]

encodeHelper [[]] 		= []
encodeHelper [x]		= [(length x, head x)]
encodeHelper (x:xs)		= (length x, head x):encodeHelper xs