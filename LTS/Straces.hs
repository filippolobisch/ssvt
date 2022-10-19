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

possibleSuspensionStates :: IOLTS -> State -> [(Label, State)]
possibleSuspensionStates iolts state =
    let (_, _, _, transitions, _) = iolts
        pStates                   = possibleStates transitions state
        isQuiescent               = quiescentCheck iolts state
    in  if isQuiescent then (delta, state) : pStates else pStates

transitionsDelta :: IOLTS -> [LabeledTransition]
transitionsDelta (states, li, lu, transitions, q) =
    [ (s, delta, s)
    | s <- states
    , quiescentCheck (states, li, lu, transitions, q) s
    ]

-- This version is the same as pDelta however the T u TDelta is not sorted.
-- This means that the delta transitions are at the end of the transitions and therefore are returned after the other transitions.
-- Therefore when checking the result of straces with simon's result (taken from Slack) it == True despite the items being in different order.
pDelta :: IOLTS -> IOLTS
pDelta (q, li, lu, ts, q0) = (q, li, lu `union` [delta], ts `union` tDelta, q0)
    where tDelta = transitionsDelta (q, li, lu, ts, q0)

lDelta :: IOLTS -> [Label]
lDelta (_, li, lu, _, _) = (li ++ lu) `union` [delta]

straces' :: IOLTS -> [(State, [Label])] -> [Trace]
straces' iolts [] = []
straces' iolts ((state, path) : xs) =
    let (_, _, _, transitions, q) = iolts
        queue =
            xs
                ++ [ (snd x, path ++ [fst x])
                   | x <- possibleSuspensionStates iolts state
                   ]
    in  path : straces' iolts queue

straces :: IOLTS -> [Trace]
straces iolts = straces' iolts [(initState, [])]
    where (s, li, lu, transitions, initState) = pDelta iolts


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
