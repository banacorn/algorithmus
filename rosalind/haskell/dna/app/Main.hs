inputPath :: FilePath
inputPath = "./data/input"

count :: Char -> String -> Int
count c = length . filter (== c)

main :: IO ()
main = do
    raw <- fmap init $ readFile inputPath
    putStrLn $ show (count 'A' raw) ++ " " ++ show (count 'C' raw) ++ " " ++ show (count 'G' raw) ++ " " ++ show (count 'T' raw)
