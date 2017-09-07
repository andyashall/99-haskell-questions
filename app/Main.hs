module Main where

import Lib

main :: IO ()
main = do
  let s = myNth ["4", "4", "2", "1", "8", "hello"] 2
  let i = revL ["4", "4", "2", "1", "8", "hello"]
  let p = isPal [1,2,2,1]
  print p

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