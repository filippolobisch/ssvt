module Traces where

import           Data.List
import           LTS_Types
import           Test.QuickCheck

-- ---------------------------------
-- Traces
-- ---------------------------------
-- This method filters the possible transitions that can occur from a passed state.
-- We return the possible transition label and output state tuples.
possibleStates :: [LabeledTransition] -> State -> [(Label, State)]
possibleStates transitions state =
  map (\(_, label, toState) -> (label, toState))
    $ filter (\(fromState, _, _) -> fromState == state) transitions

possibleStates' :: [LabeledTransition] -> State -> [(Label, State)]
possibleStates' transitions state =
  [ (l, t) | (f, l, t) <- transitions, f == state ]

checkTraceInLabels :: [Label] -> Trace -> Bool
checkTraceInLabels labels trace = trace `isInfixOf` labels

traces' :: [LabeledTransition] -> [(State, [Label])] -> [Trace]
traces' transitions []                   = []
traces' transitions ((state, path) : xs) = path : traces' transitions queue
 where
  queue =
    xs ++ [ (snd x, path ++ [fst x]) | x <- possibleStates transitions state ]

traces :: IOLTS -> [Trace]
traces (_, li, lu, transitions, q) = traces' transitions [(q, [])]
