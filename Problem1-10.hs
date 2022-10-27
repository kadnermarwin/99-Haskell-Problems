-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "No last element in empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs
-- Problem 2: Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "No last but one element in list"
myButLast [x] = error "No last but one element in list"
myButLast [x,y] = x
myButLast (_ : xs) = myButLast xs
-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: [a] -> Integer -> a
elementAt (x:_) 1 = x
elementAt (_:xs) k = if k < 1 then error "Incorrect Index" else elementAt xs (k-1)
-- Problem 4: Find the number of elements of a list.
myLengthAcc :: [a] -> Integer -> Integer
myLengthAcc [] len = len
myLengthAcc (_:xs) len = myLengthAcc xs (len + 1)
myLength :: [a] -> Integer
myLength l = myLengthAcc l 0
-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]
-- Problem 6: Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = if l == myReverse l then True else False
-- Problem 7: Flatten a nested list structure.
-- data NestedList a = Elem a | List [NestedList a]
-- flatten :: NestedList -> [a]
-- flatten a = a
-- Problem 8: Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) = if x==y then compress (y:xs) else x:compress (y:xs)
-- Problem 9: Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack :: Eq a => [a] -> [a]
pack [] = []
pack [x] = [x]
pack (x:y:xs) = if x==y then pack ((x:y) ++ xs) else x:pack (y:xs)