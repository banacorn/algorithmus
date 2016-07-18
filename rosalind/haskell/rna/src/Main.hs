module Main where

main :: IO ()
main = do
    input <- fmap init (readFile "input")
    putStrLn (map translate input)

translate :: Char -> Char
translate 'T' = 'U'
translate  c  =  c
