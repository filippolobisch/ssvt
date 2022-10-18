module PropertiesOfRelations where

import           Data.List
import           Data.Tuple
import           Relations

reflexive :: Eq a => Rel a -> Bool
reflexive = any (uncurry (==))

irreflexive :: Eq a => Rel a -> Bool
irreflexive rel = not $ reflexive rel

symmetric :: Eq a => Rel a -> Bool
symmetric r = all (\a -> swap a `elem` r) r

asymmetric :: Eq a => Rel a -> Bool
asymmetric r = all (\a -> swap a `notElem` r) r -- can be simplified to: not $ symmetric r

antisymmetric :: Eq a => Rel a -> Bool
antisymmetric r = all (\(a, b) -> (b, a) `elem` r --> a == b) r

transitive :: Eq a => Rel a -> Bool
transitive r = all (`elem` r) (r @@ r)

intransitive :: Eq a => Rel a -> Bool
intransitive r = all (`notElem` r) (r @@ r) -- can be simplified to: not $ transitive r

linear :: Eq a => Rel a -> Bool
linear r = all (\(x, y) -> (x, y) `elem` r || (y, x) `elem` r || x == y) r
-- linear r = and
--     [ (x, y) `elem` r || (y, x) `elem` r || x == y | x <- dom, y <- dom ]
--     where dom = domain r
