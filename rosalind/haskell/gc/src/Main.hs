{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    input <- fmap B.init (B.readFile "input")
    case parseOnly (many' fasta) input of
        Left  err -> print err
        Right val -> do
            let pairs = zip (map gcContent val) val
            let (p, FASTA i _) = (last $ sortOn fst pairs)
            C.putStrLn i
            C.putStrLn (C.pack $ show p)

data FASTA = FASTA ByteString ByteString
    deriving (Show)


fasta :: Parser FASTA
fasta = do
    string ">"
    i <- takeTill (== '\n')
    endOfLine
    s <- takeTill (== '>')
    return $ FASTA i (C.filter (/= '\n') s)

gcContent :: FASTA -> Double
gcContent (FASTA i s) = fromIntegral (gcLength * 100) / fromIntegral totalLength
    where   totalLength = B.length s
            gcLength = B.length (C.filter (\c -> c == 'C' || c == 'G') s)
