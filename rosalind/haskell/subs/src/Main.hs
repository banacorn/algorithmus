module Main where
-- http://www.twanvl.nl/blog/haskell/Knuth-Morris-Pratt-in-Haskell

import Data.List (intercalate)

main :: IO ()
main = do
    [haystack, needle] <- fmap lines (readFile "input")
    let result = subs needle haystack
    putStrLn (intercalate " " (map show result))

subs = subs' 0

subs' :: Eq a => Int -> [a] -> [a] -> [Int]
subs' i needle haystack = case kmp needle haystack of
    Just occ -> i + occ + 1 : subs' (i + occ + 1) needle (drop (occ + 1) haystack)
    Nothing  -> []

data KMP a = KMP
      { done :: Maybe Int
      , next :: (a -> KMP a)
      }

kmp :: Eq a => [a] -> [a] -> Maybe Int
kmp as bs = fmap minusLength $ match (makeTable as) bs
    where
        minusLength x = x - (length as)

        match table []     = done table
        match table (b:bs) = case done table of
            Just n  -> Just n
            Nothing -> fmap succ $ match (next table b) bs

        makeTable xs = table
            where   table = makeTable' xs (const table)

        makeTable' []     failure = KMP (Just 0) failure
        makeTable' (x:xs) failure = KMP Nothing  test
            where
                    test c | c == x = makeTable' xs (next (failure x))
                           | c /= x = failure c
