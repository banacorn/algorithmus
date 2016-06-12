module Data.RedBlackTree where

import Diagram

-- import Control.Applicative ((<$>), (<*>))

-- 1. A node is either red or black.
-- 2. The root is black. This rule is sometimes omitted. Since the root can always be changed from red to black, but not necessarily vice versa, this rule has little effect on analysis.
-- 3. All leaves (NIL) are black.
-- 4. If a node is red, then both its children are black.
-- 5. Every path from a given node to any of its descendant NIL nodes contains the same number of black nodes. Some definitions: the number of black nodes from the root to a node is the node's black depth; the uniform number of black nodes in all paths from root to the leaves is called the black-height of the red–black tree.[17]


data Color = R | B
    deriving (Eq)

instance Show Color where
    show R = "□"
    show B = "■"

instance Display Color where
    display c = Diagram 1 1 [show c]

data RBT a = Node Color a (RBT a) (RBT a) | Leaf
    deriving (Eq)

instance (Show a) => Display (RBT a) where
    display Leaf           = Diagram 1 1 ["_"]
    display (Node c x l r) =
        diagramNode <|>
        diagramBranch <|>
        hideLeaf l diagramL <-> spacing <-> hideLeaf r diagramR

        where   diagramNode = display c <-> string " " <-> string (show x)
                diagramBranch = string "|" <|>
                    string (replicate (width diagramL + 4) '-')
                diagramL = string "|" <|> display l
                diagramR = string "|" <|> display r
                spacing = space 3 (1 + max (height diagramL) (height diagramR))
                hideLeaf Leaf (Diagram w h _) = space w h
                hideLeaf _    diagram         = diagram

instance (Show a) => Show (RBT a) where
    show = show . display

--------------------------------------------------------------------------------
--  Query
--------------------------------------------------------------------------------

--  O(1). Is this the empty set?
null :: RBT a -> Bool
null Leaf = True
null _    = False

--  O(1). The number of elements in the set.
size :: RBT a -> Int
size Leaf = 0
size (Node _ _ l r) = 1 + size l + size r

--  O(log n). Is the element in the set?
member :: Eq a => a -> RBT a -> Bool
member x (Node _ y l r)
    | x == y = True
    | x /= y = member x l || member x r
member _ _ = False

--  O(log n). Is the element not in the set?
notMember :: Eq a => a -> RBT a -> Bool
notMember x = not . member x

--  O(log n). Find largest element smaller than the given one.
lookupLT :: Ord a => a -> RBT a -> Maybe a
lookupLT _ Leaf = Nothing
lookupLT x (Node _ y l r)
    | x <= y = lookupLT x l
    | otherwise = case lookupLT x r of
        Just y' -> Just (max y y')
        Nothing -> Just y

--  O(log n). Find smallest element greater than the given one.
lookupGT :: Ord a => a -> RBT a -> Maybe a
lookupGT _ Leaf = Nothing
lookupGT x (Node _ y l r)
    | x <= y = case lookupGT x l of
        Just y' -> Just (min y y')
        Nothing -> Just y
    | otherwise = lookupGT x r

--  O(log n). Find largest element smaller or equal to the given one.
lookupLE :: Ord a => a -> RBT a -> Maybe a
lookupLE _ Leaf = Nothing
lookupLE x (Node _ y l r)
    | x < y = lookupLE x l
    | otherwise = case lookupLE x r of
        Just y' -> Just (max y y')
        Nothing -> Just y

--  O(log n). Find smallest element greater or equal to the given one.
lookupGE :: Ord a => a -> RBT a -> Maybe a
lookupGE _ Leaf = Nothing
lookupGE x (Node _ y l r)
    | x < y = case lookupGE x l of
        Just y' -> Just (min y y')
        Nothing -> Just y
    | otherwise = lookupGE x r

-- --  O(n+m). Is this a subset? (s1 `isSubsetOf` s2) tells whether s1 is a subset of s2.
-- isSubsetOf :: Ord a => Set a -> Set a -> Bool
-- isSubsetOf Leaf _ = True
-- isSubsetOf _ Leaf = False
-- isSubsetOf (Node _ x l r) (Node _ y l' r') =


--------------------------------------------------------------------------------
--  Construction
--------------------------------------------------------------------------------

empty :: RBT a
empty = Leaf

singleton :: a -> RBT a
singleton x = Node B x Leaf Leaf

-- lemmata for insert
rotateLeftI :: RBT a -> RBT a
rotateLeftI Leaf                        = Leaf
rotateLeftI (Node _ x a Leaf)           = Node B x a              Leaf
rotateLeftI (Node _ x a (Node _ y b c)) = Node B y (Node R x a b) c

rotateRightI :: RBT a -> RBT a
rotateRightI Leaf                        = Leaf
rotateRightI (Node _ x Leaf           b) = Node B x Leaf b
rotateRightI (Node _ x (Node _ y a b) c) = Node B y a    (Node R x b c)

isRed :: RBT a -> Bool
isRed (Node R _ _ _) = True
isRed _              = False

isBlack :: RBT a -> Bool
isBlack (Node R _ _ _) = False
isBlack _              = True

adjustInsertion :: RBT a -> RBT a
adjustInsertion t@(Node B x (Node R y a b) (Node R z c d))
    | isRed a || isRed b || isRed c || isRed d =
        Node R x (Node B y a b) (Node B z c d)
    | otherwise = t
adjustInsertion t@(Node B x (Node R y a b) r)
    | isRed a = rotateRightI t
    | isRed b = rotateRightI $ Node B x (rotateLeftI (Node R y a b)) r
    | otherwise = t
adjustInsertion t@(Node B x l (Node R y a b))
    | isRed a = rotateLeftI $ Node B x l (rotateRightI (Node R y a b))
    | isRed b = rotateLeftI t
    | otherwise = t
adjustInsertion others = others

paintBlack :: RBT a -> RBT a
paintBlack Leaf = Leaf
paintBlack (Node _ x a b) = Node B x a b

insert' :: Ord a => a -> RBT a -> RBT a
insert' x Leaf = Node R x Leaf Leaf
insert' x (Node c y l r)
    | x <= y    = adjustInsertion $ Node c y (insert' x l) r
    | otherwise = adjustInsertion $ Node c y l             (insert' x r)

insert :: Ord a => a -> RBT a -> RBT a
insert x = paintBlack . insert' x

infixr 4 >:
(>:) :: Ord a => a -> RBT a -> RBT a
(>:) = insert


--------------------------------------------------------------------------------
--  Min/Max
--------------------------------------------------------------------------------

--  O(log n). The minimal element of a set.
findMin :: RBT a -> a
findMin Leaf = error "findMin: empty set has no minimal element"
findMin (Node _ x Leaf _) = x
findMin (Node _ _ l    _) = findMin l

--  O(log n). The maximum element of a set.
findMax :: RBT a -> a
findMax Leaf = error "findMxn: empty set has no maximum element"
findMax (Node _ x _ Leaf) = x
findMax (Node _ _ _ r)    = findMax r


paintRed :: RBT a -> RBT a
paintRed Leaf = Leaf
paintRed (Node _ x l r) = Node R x l r

-- lemmata for delete
rotateLeftD :: RBT a -> RBT a
rotateLeftD Leaf                          = Leaf
rotateLeftD (Node _ x l Leaf)             = Node B x l               Leaf
rotateLeftD (Node _ x l (Node _ y rl rr)) = Node B y (Node B x l rl) (paintBlack rr)


adjustDeletion :: RBT a -> RBT a
adjustDeletion others = others
-- adjustDeletion t@(Node B x _ (Node B z rl rr))
--     | isRed rr  = rotateLeftD t
--     | otherwise = t
-- adjustDeletion t@(Node R x Leaf (Node B z rl rr))
--     | isBlack rl && isBlack rr = Node B x Leaf (Node R z rl rr)
--     | otherwise = t
-- adjustDeletion t@(Node B x (Node R y Leaf Leaf) r)
--     = Node B x (Node B y Leaf Leaf) r
-- adjustDeletion t@(Node B x (Node R y Leaf Leaf) r)
--     | isRed rr  = rotateLeftD t
--     = Node B x (Node B y Leaf Leaf) r

-- O(log n). Delete an element from a set.
delete :: Ord a => a -> RBT a -> RBT a
delete _ Leaf = Leaf
delete x (Node c y l r)
    | x == y    = let (child, l', r') = replaceChildren l r in
        case child of
            Nothing -> Leaf
            Just y' -> adjustDeletion $ Node c y' l' r'
    | x <= y    = adjustDeletion $ Node c y (delete x l) r
    | otherwise = adjustDeletion $ Node c y l (delete x r)
    where
        replaceChildren Leaf Leaf = (Nothing, Leaf, Leaf)
        replaceChildren Leaf q    = (Just (findMin q), Leaf, deleteMin q)
        replaceChildren p    q    = (Just (findMax p), deleteMax p, q)

--  O(log n). Delete the minimal element. Returns an empty set if the set is empty.
deleteMin :: RBT a -> RBT a
deleteMin Leaf = empty
deleteMin (Node _ _ Leaf r) = r
deleteMin (Node c x l r) = adjustDeletion $ Node c x (deleteMin l) r

--  O(log n). Delete the maximal element. Returns an empty set if the set is empty.
deleteMax :: RBT a -> RBT a
deleteMax Leaf = empty
deleteMax (Node _ _ l Leaf) = l
deleteMax (Node c x l r) = adjustDeletion $ Node c x l (deleteMax r)
