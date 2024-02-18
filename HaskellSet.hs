module HaskellSet where

import Data.List
import qualified Data.Set as HS (fromList, toList)
import Test.QuickCheck

data Color = Red | Black deriving (Eq, Ord, Show)
data Set a = Empty | Node Color (Set a) a (Set a) deriving (Ord)

instance (Show a) => Show (Set a) where
    show s = "{" ++ init (tail (show (toList s))) ++ "}"

-- toList {2,1,4,3} => [1,2,3,4]
toList :: Set a -> [a]
toList Empty = []
toList (Node _ c1 x c2) = toList c1 ++ [x] ++ toList c2

-- fromList
fromList :: Ord a => [a] -> Set a
fromList = foldr HaskellSet.insert Empty

toFromListProp :: IO ()
toFromListProp =
    quickCheck
        ((\xs -> (HS.toList . HS.fromList $ xs) == (toList . fromList $ xs)) :: [Int] -> Bool)

-- test if two sets have the same elements (pointwise equivalent).
instance (Ord a) => Eq (Set a) where
    s1 == s2 = toList s1 == toList s2

eqProp :: IO ()
eqProp =
  quickCheck ((\xs -> (fromList . HaskellSet.toList . HaskellSet.fromList $ xs) == fromList xs) :: [Char] -> Bool)

-- the empty set
empty :: Set a
empty = Empty

-- is it the empty set?
null :: Set a -> Bool
null Empty = True
null _ = False

-- build a one element Set
singleton :: a -> Set a
singleton x = Node Black Empty x Empty

-- insert an element *x* of type *a* into Set *s* make sure there are no
-- duplicates!
insert :: (Ord a) => a -> Set a -> Set a
insert x s = makeBlack (ins s)
    where
        ins Empty = Node Red Empty x Empty
        ins (Node color c1 y c2)
            | x < y = balance color (ins c1) y c2
            | x == y = Node color c1 y c2
            | x > y = balance color c1 y (ins c2)
        makeBlack (Node _ c1 y c2) = Node Black c1 y c2

-- balance function for red-black BST
balance :: Color -> Set a -> a -> Set a -> Set a
balance Black (Node Red (Node Red c1 x c2) y c3) z c4 = Node Red (Node Black c1 x c2) y (Node Black c3 z c4)
balance Black (Node Red c1 x (Node Red c2 y c3)) z c4 = Node Red (Node Black c1 x c2) y (Node Black c3 z c4)
balance Black c1 x (Node Red (Node Red c2 y c3) z c4) = Node Red (Node Black c1 x c2) y (Node Black c3 z c4)
balance Black c1 x (Node Red c2 y (Node Red c3 z c4)) = Node Red (Node Black c1 x c2) y (Node Black c3 z c4)
balance color c1 x c2 = Node color c1 x c2

-- function to get color of node
colorOfNode :: Set a -> Color
colorOfNode Empty = Black
colorOfNode (Node c _ _ _) = c

-- join two Sets together be careful not to introduce duplicates.
union :: (Ord a) => Set a -> Set a -> Set a
union s1 s2 = setfoldr HaskellSet.insert s2 s1

-- return, as a Set, the common elements between two Sets
intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s1 s2 = setfoldr (\x acc -> if member x s2 then HaskellSet.insert x acc else acc) s1 Empty

-- all the elements in *s1* not in *s2*
-- {1,2,3,4} `difference` {3,4} => {1,2}
-- {} `difference` {0} => {}
difference :: (Ord a) => Set a -> Set a -> Set a
difference s1 s2 = setfoldr (\x acc -> if member x s2 then acc else HaskellSet.insert x acc) s1 Empty

-- is element *x* in the Set s1?
member :: (Ord a) => a -> Set a -> Bool
member x Empty = False
member x (Node color c1 y c2)
    | x < y = member x c1
    | x > y = member x c2
    | x == y = True

-- how many elements are there in the Set?
cardinality :: Set a -> Int
cardinality s = setfoldr (const (+1)) s 0

-- apply a function to every element in the Set
setmap :: (Ord b) => (a -> b) -> Set a -> Set b
setmap f Empty = Empty
setmap f (Node color c1 x c2) = HaskellSet.insert (f x) (HaskellSet.union (setmap f c1) (setmap f c2))

-- right fold a Set using a function *f*
setfoldr :: (a -> b -> b) -> Set a -> b -> b
setfoldr _ Empty acc = acc
setfoldr f (Node _ c1 x c2) acc = setfoldr f c1 (f x (setfoldr f c2 acc))

-- remove an element *x* from the set
-- return the set unaltered if *x* is not present
removeSet :: (Eq a) => a -> Set a -> Set a
removeSet x Empty = Empty
removeSet x (Node color c1 y c2)
    | x == y = mergeSets c1 c2
    | otherwise = Node color (removeSet x c1) y (removeSet x c2)
        where
            mergeSets :: Set a -> Set a -> Set a
            mergeSets Empty s2 = s2
            mergeSets s1 Empty = s1
            mergeSets s1 s2 = 
                let (minElem, s2') = deleteFindMin s2 
                in balance (colorOfNode s1) s1 minElem s2'

            deleteFindMin :: Set a -> (a, Set a)
            deleteFindMin (Node color c1 x c2)
                | HaskellSet.null c1 = (x, c2)
                | otherwise =
                    let (minElem, c1') = deleteFindMin c1
                    in (minElem, balance color c1' x c2)

-- powerset of a set
-- powerset {1,2} => { {}, {1}, {2}, {1,2} }
powerSet :: (Ord a) => Set a -> Set (Set a)
powerSet Empty = HaskellSet.fromList[empty]
powerSet s@(Node _ c1 x c2) = HaskellSet.union (setmap (HaskellSet.insert x) (powerSet (HaskellSet.removeSet x s))) (powerSet (HaskellSet.removeSet x s))
