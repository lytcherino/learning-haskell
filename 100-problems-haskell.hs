-- 2
myButLast :: [a] -> a

myButLast [] = error "empty list"
myButLast [a] = error "only a single element"
myButLast [a,b] = a
myButLast (x:xs) = myButLast xs

-- 3
-- elementAt :: [a] -> b -> a

elementAt all@(x:xs) p = elementAtHelper all p 1

elementAtHelper [] p c = error "out of bounds"
elementAtHelper (x:xs) p c 
    | p == c        = x
    | otherwise     = elementAtHelper xs p (c+1)

elementAt' (x:_) 1 = x
elementAt' [] k = error "Out of bounds"
elementAt' (_:xs) k
    | k < 1         = error " Out of bounds"
    | otherwise     = elementAt' xs (k - 1)


-- 4

myLength a = myLengthHelper a 0
    where
        myLengthHelper [] c = c
        myLengthHelper (_:xs) c = myLengthHelper xs c+1

-- 5
myReverse :: [a] -> [a]
myReverse [] = []
myReverse [a] = [a]
myReverse (x:xs) = myReverse xs ++ [x]

myReverse' :: [a] -> [a]
myReverse' list = myReverse'' list []
myReverse'' [] reversed = reversed
myReverse'' (x:xs) reversed = myReverse'' xs (x:reversed)

-- 6
last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

first' :: [a] -> a
first' (x:_) = x
first' [] = error "Empty list"

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

nfstlst [] = []
nfstlst [a] = []
nfstlst (x:xs) = nfstlst' xs []

nfstlst' [a] list = list
nfstlst' (x:xs) list = nfstlst' xs (x:list)

isPalindrome [] = True
isPalindrome [a] = True
isPalindrome all@(x:xs) 
    | first' all == last' all     = isPalindrome (nfstlst all)
    | otherwise                   = False

isP xs = xs == myReverse xs

-- 7

-- 8
compress :: (Eq a) => [a] -> [a]
compress (x:[]) = [x]
compress (x:xs)
    | x == first' xs    = compress xs 
    | otherwise         = x : compress xs 

-- 9

--pack :: (Eq a) => [a] -> [a]
pack [] = []
pack (x:ys@(y:_))
    | x == y            = x:pack ys
    | otherwise         = x:pack ys

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0        = []
take' _ []          = []
take' n (x:xs)      = x : take' (n-1) xs

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' x (y:ys)
    | x == y    = True
    | otherwise = x `elem'` ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
