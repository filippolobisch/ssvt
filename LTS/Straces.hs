module Straces where

import           After_Out_Ioco_Refuses
import           Data.List
import           LTS_Types
import           Qtraces
import           Test.QuickCheck
import           Traces


-- ---------------------------------
-- Straces
-- ---------------------------------

-- Method that gets all the possible transitions from a given state.
possibleTransitons :: [LabeledTransition] -> State -> [LabeledTransition]
possibleTransitons transitions state =
    filter (\(f, l, t) -> f == state) transitions

-- This method loops over all the transitions, when it finds a suspension state (quiescentCheck based on tretmans paper)
-- it adds a delta transition and appends the remainder of the the possibleTransitions.
-- I chose to do it this way because like this we get the delta transition ahead of the other transitions from that state (I found it easier to do it this way instead of using sorts).
transitionsDelta :: IOLTS -> [LabeledTransition]
transitionsDelta (states, li, lu, transitions, q) = concatMap
    (\s -> if quiescentCheck iolts s
        then (s, delta, s) : possibleTransitons transitions s
        else possibleTransitons transitions s
    )
    states
    where iolts = (states, li, lu, transitions, q)

-- The main function to add the delta label to both output labels and the delta transitions.
-- Creates a new IOLTS according to tretmans definition.
pDelta :: IOLTS -> IOLTS
pDelta (q, li, lu, ts, q0) = (q, li, lu `union` [delta], tDelta, q0)
    where tDelta = transitionsDelta (q, li, lu, ts, q0)

-- The input and output labels with the delta label.
-- Following tretmans definition of L*delta.
lDelta :: IOLTS -> [Label]
lDelta (_, li, lu, _, _) = (li ++ lu) `union` [delta]

-- The main straces function. Utilises the traces function that I built in the other file in preparation for the exam.
straces :: IOLTS -> [Trace]
straces iolts =
    let deltaIOLTS = pDelta iolts in [ trace | trace <- traces deltaIOLTS ]

-- Uses our previously defined traces function and compares the result with the IOLTS state array.
-- We use nub to remove any duplicate states that may be reached via different paths.
-- We also sort it to ensure the states are in their correct order (based on their int values).
-- If the arrays differ then it there is an unreachable state, however, if the are equal then all states are reachable.
hasUnreachableStates :: IOLTS -> Bool
hasUnreachableStates (q, li, lu, transitions, q0) =
    sort (nub [ s | trace <- traces iolts, s <- iolts `after` trace ]) /= q
    where iolts = (q, li, lu, transitions, q0)

-- Example Model with no unreachable states. All states are reachable.
modelNoUnreachableStates :: IOLTS
modelNoUnreachableStates =
    ([1 .. 2], ["?coin"], ["!coffee"], [(1, "?coin", 2)], 1)

-- Example Model with unreachable states (state 4)
modelUnreachableStates :: IOLTS
modelUnreachableStates =
    ( [1, 2, 3, 4]
    , ["?coin"]
    , ["!coffee"]
    , [(1, "?coin", 2), (2, "!coffee", 3)]
    , 1
    )
