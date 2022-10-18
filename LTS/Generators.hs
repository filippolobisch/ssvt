module Generators where

import           Data.List
import           LTS_Types
import           Test.QuickCheck

labelGenerator :: Gen Label
labelGenerator = do
    letters <- choose (3, 4)
    vectorOf letters $ elements ['a' .. 'z']


labelsGenerator :: [Label] -> Gen [Label]
labelsGenerator alreadyLabels = do
    ls <- listOf1 labelGenerator
    return $ nub $ ls \\ (tau : alreadyLabels)

transitionGeneratorWithoutLabel :: [State] -> Gen (State, State)
transitionGeneratorWithoutLabel states = do
    stateOne <- elements states
    stateTwo <- elements states
    return (stateOne, stateTwo)

addLabelToTransitions :: [(State, State)] -> [Label] -> [LabeledTransition]
addLabelToTransitions [] ys = []
addLabelToTransitions xs [] = []
addLabelToTransitions ((f, t) : xs) (l : ys) =
    (f, l, t) : addLabelToTransitions xs ys


ltsGenerator :: Gen LTS
ltsGenerator = do
    n      <- choose (1, 9)

    states <- vectorOf n $ elements [0 .. 9]
    let nubbedStates = sort $ nub states

    labels <- labelsGenerator []
    let nubbedLabels = nub labels ++ [tau]

    transitionsWithoutLabels <- listOf1
        $ transitionGeneratorWithoutLabel nubbedStates
    let nubTs = sort $ nub transitionsWithoutLabels

    let ts    = addLabelToTransitions nubTs (take (length nubTs) nubbedLabels)

    q0 <- elements nubbedStates
    -- let q0 = minimum nubbedStates -- Uncomment this (and comment line above) if you want the lowest int to be the starting state.
    return (nubbedStates, nubbedLabels, ts, q0)


ioltsGenerator :: Gen IOLTS
ioltsGenerator = do
    n      <- choose (1, 9)

    states <- vectorOf n $ elements [0 .. 9]
    let nubbedStates = sort $ nub states

    inputLabels <- labelsGenerator []
    let nubbedInputLabels = nub inputLabels ++ [tau]

    outputLabels <- labelsGenerator nubbedInputLabels
    let nubbedOutputLabels = nub outputLabels

    transitionsWithoutLabels <- listOf1
        $ transitionGeneratorWithoutLabel nubbedStates
    let nubTs = sort $ nub transitionsWithoutLabels

    let ts = addLabelToTransitions
            nubTs
            (take (length nubTs) (nubbedInputLabels ++ nubbedOutputLabels))

    q0 <- elements nubbedStates
    -- let q0 = minimum nubbedStates -- Uncomment this (and comment line above) if you want the lowest int to be the starting state.
    return (nubbedStates, nubbedInputLabels, nubbedOutputLabels, ts, q0)

propLTSGenTest :: LTS -> Bool
propLTSGenTest lts = lts == lts
