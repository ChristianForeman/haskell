double :: Num a => a -> a
double x = x * 2

quadruple :: Num a => a -> a
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

getLastV1 xs = head (reverse xs)

getLastV2 xs = xs !! (length xs - 1)

dropLastV1 xs = take (length xs - 1) xs

dropLastV2 xs = reverse (tail (reverse xs))

secondV1 :: [a] -> a
secondV1 xs = head (tail xs)

swapV1 :: (a,b) -> (b,a)
swapV1 (x,y) = (y,x)

pairV1 :: a -> b -> (a,b)
pairV1 x y = (x, y)

palindrome :: Eq a => [a] -> Bool
palindrome xs = reverse xs == xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

odds n = map (\x -> x * 2 + 1) [0..n]


--implement safetail in 3 ways
safetailV1 xs = if null xs then [] else tail xs

safetailV2 xs | null xs = []
              | otherwise = tail xs

safetailV3 [] = []
safetailV3 xs = tail xs

-- three definitions for the || or operator
-- V1
{-
True  || True  = True
True  || False = True
False || True  = True
False || False = False

-- V2
False || False = False 
_     || _     = True

-- V3
True  || _ = True 
False || b = b
-}

-- Implement && with conditionals
-- V1
{-
(&&) a b = if a then
            if b then True else False 
            else False

-- V2
(&&) a b = if a then b else False
-}

factors n = [x | x <- [1..n], n `mod` x == 0]

prime n = factors n == [1,n]

primes n = [x | x <- [1..n], prime x]

-- pyths returns list of pyth triples up until n
pythTriples n = [(x,y,n) | x <- [1..n], y <- [1..n], x^2 + y^2 == n^2]

pyths n = [x | val <- [1..n], x <- pythTriples val]

-- list of perfect numbers
isPerfect n = sum (init (factors n)) == n

perfects n = [x | x <- [1..n], isPerfect x]

-- does scalar product of two lists, assumes same length of vectors
scalarProductV1 xs ys = sum [xs !! i * ys !! i | i <- [0..n-1]]
                        where n = length xs

scalarProductV2 xs ys = sum [x * y | (x,y) <- zip xs ys]


-- FP 8 Recursive Functions Excercises
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (x:xs) = x && myAnd xs

-- foldr implementation
myAndV2 :: [Bool] -> Bool
myAndV2 = foldr (&&) True

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs: xss) = xs ++ myConcat xss

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n x = x : myReplicate (n - 1) x

-- my impl of !!
bangBang :: [a] -> Int -> a
bangBang (x:_) 0 = x
bangBang (_:xs) n = bangBang xs (n - 1) 

-- find if something is an element of a list
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False 
myElem m (x:xs) = (m == x) || (myElem m xs)

-- insertionSort
myInsert :: Int -> [Int] -> [Int]
myInsert x [] = [x]
myInsert x (y:ys) = if x <= y then
                        x:y:ys
                    else
                        y : myInsert x ys

myInsertionSort :: [Int] -> [Int]
myInsertionSort [] = []
myInsertionSort (x:xs) = myInsert x (myInsertionSort xs)

-- Merge sort
myMerge :: Ord a => [a] -> [a] -> [a]
myMerge [] [] = []
myMerge xs [] = xs
myMerge [] ys = ys
myMerge (x:xs) (y:ys) | x < y     = x : myMerge xs (y:ys)
                      | otherwise = y : myMerge (x:xs) ys

myHalve :: [a] -> ([a],[a])
myHalve [] = ([],[])
myHalve xs = splitAt (fst (l `divMod` 2)) xs
                where l = length xs


myMSort :: Ord a => [a] -> [a]
myMSort [] = []
myMSort [x] = [x]
myMSort xs = myMerge (myMSort ys) (myMSort zs)
                where (ys, zs) = myHalve xs


-- Types and Data
data Nat = Zero | Succ Nat


add :: Nat -> Nat -> Nat
add Zero m = m
add (Succ n) m = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero     m = Zero
mult (Succ n) m = add (mult n m) m

data Tree a = Leaf a | Node (Tree a) (Tree a)