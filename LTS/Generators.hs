module Generators where

import           Data.List
import           LTS_Types
import           Test.QuickCheck

labelGenerator :: Gen Label
labelGenerator = do
    letters <- choose (3, 4)
    vectorOf letters $ elements ['a' .. 'z']


labelsGenerator :: [Label] -> Gen [Label]
labelsGenerator labelsToExclude = do
    ls <- listOf1 labelGenerator
    return $ nub $ ls \\ (tau : labelsToExclude)


-- Labels for IOLTS
ioltsLabelGenerator :: Gen Label
ioltsLabelGenerator = do
    inputOutput <- choose ('?', '!')
    letters     <- choose (3, 4)
    label       <- vectorOf letters $ elements ['a' .. 'z']
    return $ inputOutput : label


ioltsLabelsGenerator :: [Label] -> Gen [Label]
ioltsLabelsGenerator labelsToExclude = do
    ls <- listOf1 ioltsLabelGenerator
    return $ nub $ ls \\ (tau : labelsToExclude)




stateTransitionGenerator :: [State] -> Gen (State, State)
stateTransitionGenerator states = do
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
    let statesSet = sort $ nub states

    labels <- labelsGenerator []
    let uniqueLabels = nub labels ++ [tau]

    -- State transitions contains from and to states without the label in the middle of the tuple.
    stateTransitions <- listOf1 $ stateTransitionGenerator statesSet
    let uniqueStateTransitions = sort $ nub stateTransitions
    let lengthStateTs          = length uniqueStateTransitions
    let labelsForTs            = take lengthStateTs uniqueLabels

    let transitions = addLabelToTransitions uniqueStateTransitions labelsForTs
    let q0                     = head statesSet
    return (statesSet, labelsForTs, transitions, q0)


filterLabels :: [LabeledTransition] -> [Label] -> [Label]
filterLabels ts labels =
    map (\(_, l, _) -> l) $ filter (\(_, l, _) -> l `elem` labels) ts

ioltsGenerator :: Gen IOLTS
ioltsGenerator = do
    n      <- choose (1, 9)

    states <- vectorOf n $ elements [0 .. 9]
    let statesSet = sort $ nub states

    labels <- ioltsLabelsGenerator []
    let uniqueLabels = nub labels ++ [tau]

    let inputLabels = map tail $ filter (\l -> head l == '?') uniqueLabels
    let outputLabels = map tail $ filter (\l -> head l == '!') uniqueLabels

    -- State transitions contains from and to states without the label in the middle of the tuple.
    stateTransitions <- listOf1 $ stateTransitionGenerator statesSet
    let uniqueStateTransitions = sort $ nub stateTransitions
    let lengthStateTs          = length uniqueStateTransitions
    let labelsForTs = take lengthStateTs (inputLabels `union` outputLabels)

    let transitions = addLabelToTransitions uniqueStateTransitions labelsForTs
    let inputLabelsInTs        = filterLabels transitions inputLabels
    let outputLabelsInTs       = filterLabels transitions outputLabels

    let q0                     = head statesSet
    return (statesSet, inputLabelsInTs, outputLabelsInTs, transitions, q0)


propLTSGenTest :: LTS -> Bool
propLTSGenTest lts = lts == lts

propIOLTSGenTest :: IOLTS -> Bool
propIOLTSGenTest lts = lts == lts
