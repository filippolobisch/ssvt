module Mutation where

import           Data.List
import           Debug.Trace
-- import           Mutation
import           Test.QuickCheck

-- Implemented mutations only work on entire list and don't mutate for
-- example individual elements of the list.

-- Other forms of mutations:
permutate :: [Integer] -> Gen [Integer]
permutate ls = do
    per <- shuffle ls
    if per /= ls then return per else permutate ls

multiplyRandom' :: [Integer] -> Gen [Integer]
multiplyRandom' ls = do
    random <- arbitrary :: Gen Integer
    let new = fmap (* random) ls
    if new /= ls then return new else multiplyRandom' ls

multiplyRandomElement :: [Integer] -> Gen [Integer]
multiplyRandomElement ls = do
    random <- arbitrary :: Gen Integer
    index  <- choose (0, length ls - 1)
    let modifiedList = fmap
            (\x -> if elemIndex x ls == Just index then x * random else x)
            ls
    if random /= 1 then return modifiedList else multiplyRandomElement ls

reverseElements' :: [Integer] -> Gen [Integer]
reverseElements' xs = return $ reverse xs

negateElements' :: [Integer] -> Gen [Integer]
negateElements' xs = return $ map negate xs

propTest :: [Integer] -> Bool
propTest ls = ls /= [1, 2, 3, 4, 5]

-- Main Exercise 1 function
------------------------------------------------
mutationMain :: IO ()
mutationMain = do
    quickCheck $ forAll (multiplyRandomElement [1, 2, 3, 4, 5]) propTest
