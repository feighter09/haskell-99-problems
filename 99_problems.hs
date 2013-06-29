
-- Problem 5
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs
	| len <= 1 	= True
	| otherwise	= (head xs) == (last xs) && isPalindrome (belly xs)
	where len = length xs

belly :: [a] -> [a]
belly ls = tail (init ls)