module Hoare where

import           Data.List
import           System.Random
import           Test.QuickCheck


-- ---------------------------------
-- LTS Lab Source
-- ---------------------------------
infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

forall :: [a] -> (a -> Bool) -> Bool
forall = flip all

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = forall xs (\x -> p x --> q x)
weaker xs p q = stronger xs q p

-- ---------------------------------
-- End LTS Lab Source
-- ---------------------------------
