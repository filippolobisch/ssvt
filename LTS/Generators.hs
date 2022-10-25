module Generators where

import           Data.List
import           LTS_Types
import           Test.QuickCheck

-- Taken from Lab group9 of last year which I was a part of.

-- ---------------------------------
-- Label Generator
-- ---------------------------------
labelGen :: Gen Label
labelGen = vectorOf 3 $ elements ['a' .. 'z']

-- ---------------------------------
-- Labels Generator
-- ---------------------------------
labelsGen :: [Label] -> Gen [Label]
labelsGen not = do
    ls <- listOf1 labelGen
    return $ nub $ ls \\ (tau : not)

-- ---------------------------------
-- lts Generator
-- ---------------------------------
ltsGen :: Gen IOLTS
ltsGen = ltsGen' True

-- ---------------------------------
-- lts (without loops) Generator
-- ---------------------------------
ltsNoLoopGen :: Gen IOLTS
ltsNoLoopGen = ltsGen' False

-- ---------------------------------
-- lts Generator body
-- ---------------------------------

-- The ltsGen body function that can decide whether or
-- not to allow loops inside the traces. This is done by
-- making sure a LabeledTransition can never go to itself
-- and a State can never go to a State that is smaller (int)
-- than itself.
ltsGen' :: Bool -> Gen IOLTS
ltsGen' allowLoop = do
    n  <- choose (1, 9)
    q1 <- vectorOf n (elements [0 .. 9])
    li <- labelsGen []
    lu <- labelsGen li
    let q = nub q1

    t1 <- listOf1 $ labeledTransitionGen q (li ++ lu)

    let t = filterTransitions allowLoop t1

    q0 <- elements q
    return (q, li, lu, t, q0)

-- Filtering the [LabeledTransition] list to remove transitions that
-- go from a state to the same state of go from a state to a lower state.
-- This can be used to create a LTS without unending traces.
filterTransitions :: Bool -> [LabeledTransition] -> [LabeledTransition]
filterTransitions True t1 = nub t1
filterTransitions False t1 =
    filter (\(s1, _, s2) -> s1 /= s2 && s1 > s2) (nub t1)

-- ---------------------------------
-- LabeledTransition Generator
-- ---------------------------------
labeledTransitionGen :: [State] -> [Label] -> Gen LabeledTransition
labeledTransitionGen q ls = do
    x <- elements q
    y <- elements q
    l <- elements (tau : ls)
    return (x, l, y)
