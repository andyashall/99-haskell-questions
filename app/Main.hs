module Main where

import Lib

import Data.List (group)

data NestedList a = Elem a | List [NestedList a]

data ListItem a = Single a | Multiple (Int, a)
    deriving (Show)

main :: IO ()
main = do
  let s = myNth ["4", "4", "2", "1", "8", "hello"] 2
  let i = revL ["4", "4", "2", "1", "8", "hello"]
  let p = isPal [1,2,2,1]
  let f = flat (List [Elem 5, List [Elem 4, List [Elem 7, Elem 2], Elem 9]])
  let c = lengM [1,1,1,3,9,3,3,5,4,4,4,5]
  print c

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
lengM xs = map (\x -> case length x of
  1 -> Single x
  _ -> Multiple (length x, head x))
  (group xs)