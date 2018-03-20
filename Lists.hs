-- find the last element of a list
last' :: [a] -> a
last' [] = error "Empty list"
-- base case here, if only one element then x
last' [x]= x
last' (x:xs) = last' xs


-- find the last but one element of a list
secondToLast :: [a] -> a
secondToLast [] = error "Empty list provided"
secondToLast [x] = error "List with too few arguments"
-- the case of a list of x and anything, that is the base
secondToLast [x, _] = x
secondToLast (_:xs) = secondToLast xs

-- third last element of a list
thirdToLast :: [a] -> a
-- construct a list of three elements, return the first of them, this is the base case | terminating case
thirdToLast [x, y, _] = x
thirdToLast (_:xs) = thirdToLast xs


-- find the k'th element of a list
-- one possible solution is to get a list of length k
-- the last element is the k'th element
elemenAt :: [a] -> Int -> a
elemenAt (x:_) 1 = x
elemenAt [] _    = error "Empty List"
elemenAt (_:xs) k
  | k < 1         = error "Index out of bounds"
  | otherwise     = elemenAt xs (k - 1)


-- find the number of elements in a list
len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs


-- reverse a list
rev :: [a] -> [a]
rev [] = []
rev [x] = [x]
-- x needs to be a list as ++ expects a list 
rev (x:xs) =  rev xs ++ [x]


-- find if a list palindrome can be read in reverse the same
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome lst
  | lst  == rev lst      = True
  | otherwise            = False
