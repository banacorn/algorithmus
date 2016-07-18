module Main where

main :: IO ()
main = do
    input <- fmap init (readFile "input")
    putStrLn $ reverse $ map complement input

complement :: Char -> Char
complement 'A' = 'T'
complement 'C' = 'G'
complement 'G' = 'C'
complement 'T' = 'A'
