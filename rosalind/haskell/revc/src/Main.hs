module Main where

main :: IO ()
main = do
    input <- fmap init (readFile "input")
    putStrLn $ reverse $ map revc input

revc :: Char -> Char
revc 'A' = 'T'
revc 'C' = 'G'
revc 'G' = 'C'
revc 'T' = 'A'
