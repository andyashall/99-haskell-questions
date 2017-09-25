module Main where

import Lib

import Data.List (group)

data NestedList a = Elem a | List [NestedList a]

data ListItem a = Single a | Multiple Int a
    deriving (Show)

main :: IO ()
main = do
  let s = myNth ["4", "4", "2", "1", "8", "hello"] 2
  let i = revL ["4", "4", "2", "1", "8", "hello"]
  let p = isPal [1,2,2,1]
  let f = flat (List [Elem 5, List [Elem 4, List [Elem 7, Elem 2], Elem 9]])
  let c = lengM [1,1,1,3,9,3,3,5,4,4,4,5]
  let d = decodeM [Multiple 3 1,Single 3,Single 9,Multiple 2 3,Single 5,Multiple 3 4,Single 5]
  let du = dropN [1,2,3,4] 3
  let sp = split "abcdefghik" 3
  let sl = slice [1,2,3,4,5,6,7,8,9] 2 5
  print sl

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

-- 6. Is a list a palindrome
isPal :: (Eq a) => [a] -> Bool
isPal x = x == (reverse x)

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