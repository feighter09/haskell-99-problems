-- Problem 1
--(*) Find the last element of a list.
myLast :: [a] -> a
myLast [] 		= error "Can't call last on an empty list"
myLast [x] 		= x
myLast (_:xs) = myLast xs


-- Problem 2
--(*) Find the last but one element of a list.
myLastButOne :: [a] -> a
myLastButOne [] 		= error "Can't call last on a list with less than 2 elements"
myLastButOne [x] 		= error "Can't call last on a list with less than 2 elements"
myLastButOne [x,_] 	= x
myLastButOne (_:xs) = myLastButOne xs


-- Problem 3
--(*) Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Int -> a
elementAt xs k
	| k < 0			= error "Index out of range"
	| len < k 	= error "Index out of range"
	| otherwise = xs !! (k - 1)
	where len 	= length xs


-- Problem 4
--(*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] 		= 0
myLength (_:xs) = 1 + myLength xs


-- Problem 5
--(*) Reverse a list.
myReverse :: [a] -> [a]
myReverse [] 			= []
myReverse [x] 		= [x]
myReverse (x:xs) 	= (last xs) : (myReverse(init xs) ++ x:[])


-- Problem 6
--(*) Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
	| length xs <= 1 	= True
	| otherwise				= (head xs) == (last xs) && isPalindrome (belly xs)
	where belly ls = tail (init ls)


-- ************************************************** Problem 7 **************************************************
--(**) Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]
-- **************************************************


-- Problem 8
--(**) Eliminate consecutive duplicates of list elements
compress :: (Eq a) => [a] -> [a]
compress [] 			= []
compress [a] 			= [a]
compress (x:xs)
	| x == head xs	= compress xs
	| otherwise			= x:compress xs


-- Problem 9
--(**) Pack consecutive duplicates of list elements into sublists. 
-- If a list contains repeated elements they should be placed in separate sublists
pack :: (Eq a) => [a] -> [[a]]

pack [] 	= []
pack [a] 	= [[a]]
pack xs		= reps : pack rest 
	where (reps, rest) = span (== head xs) xs

packHelper :: (Eq a) => [[a]] -> [a] -> [[a]]

packHelper x []						= x
packHelper [[]] (x:xs) 		= packHelper [[x]] xs
packHelper x (y:xs)
	| last (last x) == y		= packHelper (init x ++ [y:last x]) xs
	| otherwise 						= packHelper (x ++ [[y]]) xs


-- Problem 10
--(*) Run-length encoding of a list. Use the result of problem P09 to implement 
-- the so-called run-length encoding data compression method. Consecutive 
-- duplicates of elements are encoded as lists (N E) where N is the number of
-- duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = encodeHelper (pack xs)

encodeHelper :: [[a]] -> [(Int, a)]

encodeHelper [] 			= []
encodeHelper [x]			= [(length x, head x)]
encodeHelper (x:xs)		= (length x, head x):encodeHelper xs


-- Problem 14
--(*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli [] = []
dupli [x] = [x,x]
dupli (x:xs) = [x,x] ++ dupli xs


-- Problem 15
--(**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli [] _ = []
repli _ n
	| n < 0		= error "Can't replicate negative times"
	| n == 0	= []
repli (x:xs) n = dups ++ repli xs n 
	where dups = take n (repeat x)
-- Wow so much better sol'n:
--repli xs n = concatMap (replicate n) xs			-- applies the first arg to each elt of second arg, concatenates results (i guess)


-- Problem 16
--(**) Drop every N'th element from a list
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs x
	| (length xs) < x = xs
	| otherwise				= init front ++ dropEvery back x
	where (front, back) = split xs x


-- Problem 17
--(*) Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)


-- Problem 18
--(**) Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice xs x y
	| x <= 0		= slice xs 1 y
	| otherwise	= snd (split (fst (split xs y)) (x - 1))


-- Problem 19
--(**) Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs n
	| n > 0			= rotate (middle ++ last:[first]) (n - 1)
	| n < 0			= rotate (last:first:middle) (n + 1)
	| otherwise	= xs
	where 
		first 	= xs !! 0
		middle	= tail (init xs)
		last		= xs !! (length xs - 1)


-- Problem 20
--(*) Remove the K'th element from a list.
removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt xs n
	| n < 0					= error "Trying to remove an element out of range"
	| n > length xs	= error "Trying to remove an element out of range"
	| otherwise 		= (init front) ++ back
	where (front, back) = split xs n


-- Problem 21
-- Insert an element at a given position into a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt elt list spot
	| spot < 1						= error "Trying to insert an element out of range"
	|	spot >= length list	= error "Trying to insert an element out of range"
	| otherwise						= front ++ [elt] ++ back
	where (front, back) = split list (spot - 1)


-- Problem 22
-- Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range first last
	| first < last	= first:(range (first + 1) last)
	| first > last	= first:(range (first - 1) last)
	| otherwise 		= [first]


-- Problem 23
-- Extract a given number of randomly selected elements from a list.
rnd_select :: [a] -> Int -> IO [a]
rnd_select [] _ = return []








