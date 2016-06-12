module Main where

import Data.RedBlackTree

insertAll :: [Int] -> RBT Int
insertAll = foldr insert empty

ll :: RBT Int
ll = 4 >: 3 >: 2 >: 1 >: empty

a :: RBT Int
a = insertAll [9, 8 .. 2]

-- b :: RBT Int
-- b = deleteMin $ insertAll [9, 8 .. 2]
--
-- c :: RBT Int
-- c = deleteMin $ deleteMin $ insertAll [9, 8 .. 2]

-- ll :: RBT Int
-- ll = 6 >: 8 >: 10 >: empty
-- lr :: RBT Int
-- lr = 8 >: 6 >: 10 >: empty
-- rr :: RBT Int
-- rr = 10 >: 8 >: 6 >: empty
-- rl :: RBT Int
-- rl = 8 >: 10 >: 6 >: empty

main :: IO ()
main = print "hello"
