module PropertiesOfRelations where

import           Data.List
import           Data.Tuple
import           Relations
-- import           SetOrd


-- Georgia's helper
isTransitive :: Eq a => Rel a -> Bool
isTransitive x = and [ (a, d) `elem` x | (a, b) <- x, (c, d) <- x, b == c ]

isReflexive :: Eq a => Rel a -> [a] -> Bool
isReflexive r = all (\x -> (x, x) `elem` r)

isAntisymmetric :: Eq a => Rel a -> Bool
isAntisymmetric r = and [ (y, x) `notElem` r || x == y | (x, y) <- r ]
-- End Georgia's helper

isIrreflexive :: Eq a => Rel a -> [a] -> Bool
isIrreflexive r a = not $ all (\x -> (x, x) `elem` r) a

isSymmetric :: Eq a => Rel a -> Bool
isSymmetric r = and [ (y, x) `elem` r | (x, y) <- r ]
-- isSymmetric r = all (\(x, y) -> (y, x) `elem` r) r

isAsymmetric :: Eq a => Rel a -> Bool
isAsymmetric r = and [ (y, x) `notElem` r | (x, y) <- r ]

isIntransitive :: Eq a => Rel a -> Bool
isIntransitive r =
    and [ (a, d) `notElem` r | (a, b) <- r, (c, d) <- r, b == c ]

linear :: Eq a => Rel a -> Bool
linear r = all (\(x, y) -> (x, y) `elem` r || (y, x) `elem` r || x == y) r
-- linear r = and
--     [ (x, y) `elem` r || (y, x) `elem` r || x == y | x <- dom, y <- dom ]
--     where dom = domain r


-- Haskell road: A relation R on A is a pre-order (or quasi-order) if R is transitive and reflexive.
quasiOrder, preOrder :: Eq a => Rel a -> [a] -> Bool
quasiOrder r a = isTransitive r && isReflexive r a
preOrder r a = isTransitive r && isReflexive r a

-- Haskell road: A relation R on A is a strict partial order if R is transitive and irreflexive.
strictPartialOrder :: Eq a => Rel a -> [a] -> Bool
strictPartialOrder r a = isTransitive r && isIrreflexive r a -- && isAsymmetric r && isAntisymmetric r

-- Haskell road: A relation R on A is a partial order if R is transitive, reflexive and antisymmetric.
partialOrder :: Eq a => Rel a -> [a] -> Bool
partialOrder r a = isTransitive r && isReflexive r a && isAntisymmetric r

-- Haskell road: A partial order that is also linear is called a total order.
totalOrder :: Eq a => Rel a -> [a] -> Bool
totalOrder r a = partialOrder r a && linear r

equivalence :: Eq a => Rel a -> [a] -> Bool
equivalence r a = isReflexive r a && isSymmetric r && isTransitive r

