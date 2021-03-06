module Main where

import Lib

import Data.List (group, intersperse)
import Data.Char (digitToInt)

data NestedList a = Elem a | List [NestedList a]

data ListItem a = Single a | Multiple Int a
    deriving (Show)

main :: IO ()
main = do
  let s = myNth ["4", "4", "2", "1", "8", "hello"] 2
      i = rev2 ["4", "4", "2", "1", "8", "hello"]
      p = isPal [1,2,2,1]
      f = flat (List [Elem 5, List [Elem 4, List [Elem 7, Elem 2], Elem 9]])
      c = lengM [1,1,1,3,9,3,3,5,4,4,4,5]
      d = decodeM [Multiple 3 1,Single 3,Single 9,Multiple 2 3,Single 5,Multiple 3 4,Single 5]
      du = dropN [1,2,3,4] 3
      sp = split "abcdefghik" 3
      sl = slice [1,2,3,4,5,6,7,8,9] 2 5
      fw = fullWords 576
      qs = quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9,11,43,34,65,52,76,43,12,54,729,77,44,10]
      rep = replicate' 5 "A"
      rec = recTen 100
      dt = applyTwice divTen 50
      zw = zipWith' (+) [4,2,5,6] [2,6,2,3] 
  print zw

-- 1. Get the last item in a list
myLast :: [a] -> a
myLast x = do
  let i = (length x) - 1
  x!!i

-- 2. Get the second last item in a list
mySLast :: [a] -> a
mySLast x = do
  let i = (length x) - 2
  x!!i

-- 3. Get the nth item in a list
myNth :: [a] -> Int -> a
myNth x y = x!!(y-1)

-- 4. Get the n of items in a list
listN :: [a] -> Int
listN x = length x

-- 5. Reverse list
revL :: [a] -> [a]
revL x = reverse x

rev2 :: [a] -> [a]
rev2 [] = []
rev2 (x:xs) = rev2 xs ++ [x]

-- 6. Is a list a palindrome
isPal :: (Eq a) => [a] -> Bool
isPal x = x == (rev2 x)

-- 7. Flatten nested list
flat :: NestedList a -> [a]
flat (Elem x) = [x]
flat (List x) = concatMap flat x

-- 8. Remove duplicated elements
compress :: (Eq a) => [a] -> [a]
compress = map head . group

-- 9. Pack consecutive duplicates of list elements into sublists
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack [x] = [[x]]
pack (x:xs) = if x `elem` (head (pack xs))
              then (x:(head (pack xs))):(tail (pack xs))
              else [x]:(pack xs)

-- 10. Run-length encoding of a list
leng :: (Eq a) => [a] -> [(Int, a)]
leng xs = map (\x -> (length x,head x)) (group xs)

-- 11. Run-length encoding of a list with singles
lengM :: (Eq a) => [a] -> [ListItem a]
lengM xs = [y | x <- group xs, let y = if (length x) == 1 then Single (head x) else Multiple (length x) (head x)]

-- 12; Decode Run-length encoding of a list with singles
decodeM :: [ListItem a] -> [a]
decodeM = concatMap decodeHelper
    where
      decodeHelper (Single x)     = [x]
      decodeHelper (Multiple n x) = replicate n x

-- 14. Duplicate elms in list
dupE :: (Eq a) => [a] -> [a]
dupE = concatMap (\x -> [x,x])

-- 15. Replicate Elms x times
repE :: [a] -> Int -> [a]
repE xs n = concatMap (replicate n) xs

-- 16. Drop every nth elem in list
dropN :: [a] -> Int -> [a]
dropN xs n
  | length xs < n = xs
  | otherwise     = take (n-1) xs ++ dropN (drop n xs) n

-- 17. Split list into 2
split :: [a] -> Int -> ([a], [a])
split []         _             = ([], [])
split l@(x : xs) n | n > 0     = (x : ys, zs)
                   | otherwise = ([], l)
    where (ys,zs) = split xs (n - 1)

-- 18. Slice list 
slice :: [a] -> Int -> Int -> Maybe [a]
slice [] _ _ = Just []
slice xs k n  | k == n = Just []
    | k > n || k > length xs || 
                  n > length xs || k < 0 || n < 0 = Nothing
    | k == 0 = Just (take n xs)
    | otherwise = Just (drop (k-1) $ take n xs)

fullWords :: Integer -> String
fullWords n = concat $ intersperse "-" [digits!!digitToInt d | d <- show n]
  where digits = ["zero", "one", "two", "three", "four",
                  "five", "six", "seven", "eight", "nine"]

quicksort :: (Ord a) => [a] -> [a]  
quicksort [] = []  
quicksort (x:xs) =   
    let smallerSorted = quicksort [a | a <- xs, a <= x]  
        biggerSorted = quicksort [a | a <- xs, a > x]  
    in  smallerSorted ++ [x] ++ biggerSorted  


maximum' :: (Ord a) => [a] -> a  
maximum' [] = error "maximum of empty list"  
maximum' [x] = x  
maximum' (x:xs)   
    | x > maxTail = x  
    | otherwise = maxTail  
    where maxTail = maximum' xs  

replicate' :: (Num i, Ord i) => i -> a -> [a]  
replicate' n x  
    | n <= 0    = []  
    | otherwise = x:replicate' (n-1) x  

recTen :: Int -> Int
recTen x
  | x == 10 = x
  | x < 10 = recTen (x+1)
  | x > 10 = recTen (x-1)

applyTwice :: (a -> a) -> a -> a  
applyTwice f x = f (f x) 

divTen :: (Floating a) => a -> a
divTen = (/10)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]  
zipWith' _ [] _ = []  
zipWith' _ _ [] = []  
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys 

-- fib :: Int -> Int -> Int -> Int
-- fib s n m
--   | n == 0 = n
--   | s == m = n
--   | otherwise = fib (s+1) (n * (n+1)) m

-- fibs :: [Integer]
-- fibs = 0 : 1 : zipWith (+) fibs (tail fibs)