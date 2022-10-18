module Relations where

import           Data.List
import           SetOrd

type Rel a = [(a, a)]

-- Infix method from Lab2.
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x, z) | (x, y) <- r, (w, z) <- s, y == w ]

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set []) set2 = set2
setIntersection (Set (x : xs)) set2 =
    insertSet x (setIntersection (Set xs) set2)

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set []      ) set2 = set2
setUnion (Set (x : xs)) set2 = insertSet x (setUnion (Set xs) set2)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set []      ) set2 = set2
setDifference (Set (x : xs)) set2 = insertSet x (setDifference (Set xs) set2)


-- SymClos
symClos :: Ord a => Rel a -> Rel a
symClos rel = nub $ concatMap (\(x, y) -> [(x, y), (y, x)]) (nub rel)

domain :: Eq a => Rel a -> [a]
domain rel = nub $ map fst rel

range :: Eq a => Rel a -> [a]
range rel = nub $ map snd rel


identity :: Eq a => [a] -> Rel a
identity x = nub $ [ (v, v) | v <- x ]

inverse :: Eq a => Rel a -> Rel a
inverse r = nub $ [ (b, a) | (a, b) <- r ] -- could also be done as: map swap

-- Serial
-- Function that checks whether a given relation is serial for the given domain.
-- For a relation to be serial every point x from the domain there must be a y,
-- where (x,y) is present in the relation.
isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial set rel = all (\x -> any (\y -> (x, y) `elem` rel) set) set

-- fixed point method taken from Lecture Slides
fp :: Eq a => (a -> a) -> a -> a
fp f = until (\x -> x == f x) f

trClos :: Ord a => Rel a -> Rel a
trClos = sort . nub . fp (\r -> nub (r ++ (r @@ r)))


symVsTrClos :: Ord a => Rel a -> (Rel a, Rel a)
symVsTrClos rel = (symClos $ trClos rel, trClos $ symClos rel)
-- Tested using [(1,2),(2,3),(3,4)]
-- symClos of trClos = [(1,2),(2,1),(1,3),(3,1),(1,4),(4,1),(2,3),(3,2),(2,4),(4,2),(3,4),(4,3)]
-- trClos of symClos = [(1,1),(1,2),(1,3),(1,4),(2,1),(2,2),(2,3),(2,4),(3,1),(3,2),(3,3),(3,4),(4,1),(4,2),(4,3),(4,4)]
-- first one is missing the identity matrix which arises from elements such as (1,2) and (2,1) both being in the relation.
