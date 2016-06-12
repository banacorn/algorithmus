module Diagram (
        Diagram(..),
        space,
        string,
        pad,
        composeH, (<->),
        composeV, (<|>),
        Display(..)
    ) where

import Data.List (intersperse)

data Diagram = Diagram
    {   width :: Int
    ,   height :: Int
    ,   content :: [String]
    }

instance Show Diagram where
    show (Diagram _ _ d) = concat (intersperse "\n" d)

space' :: Int -> Int -> [String]
space' w h = replicate h (replicate w ' ')

space :: Int -> Int -> Diagram
space w h = Diagram w h (space' w h)

string :: String -> Diagram
string s = Diagram (length s) 1 [s]

pad :: Int -> Int -> Int -> Int -> Diagram -> Diagram
pad t r b l (Diagram w h d) = Diagram w' h' d'
    where   w' = l + w + r
            h' = t + h + b
            d' = top ++ mid ++ bot
            top = space' w' t
            mid = map (\s -> replicate l ' ' ++ s ++ replicate r ' ') d
            bot = space' w' b

infixl 5 <->
infixl 4 <|>

(<->) :: Diagram -> Diagram -> Diagram
(<->) = composeH
(<|>) :: Diagram -> Diagram -> Diagram
(<|>) = composeV

composeH :: Diagram -> Diagram -> Diagram
composeH (Diagram w1 h1 d1) (Diagram w2 h2 d2) =
    Diagram (w1 + w2) (max h1 h2) d'
    where   diff = abs (h1 - h2)
            d' = if h1 > h2
                then map (uncurry (++)) (zip d1 (d2 ++ space' w2 diff))
                else map (uncurry (++)) (zip (d1 ++ space' w1 diff) d2)

composeV :: Diagram -> Diagram -> Diagram
composeV (Diagram w1 h1 d1) (Diagram w2 h2 d2) =
    Diagram (max w1 w2) (h1 + h2) d'
    where   diff = abs (w1 - w2)
            d' = if w1 > w2
                then d1 ++ (map (flip (++) (replicate diff ' ')) d2)
                else (map (flip (++) (replicate diff ' ')) d1) ++ d2

-- t0 :: Diagram
-- t0 = Diagram 3 3 ["***", "===", "***"]
--
-- t1 = Diagram 2 2 ["__", "__"]

class Display a where
    display :: a -> Diagram
