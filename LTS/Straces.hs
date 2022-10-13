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
possibleSuspensionStates iolts state | isQuiescent = (delta, state) : pStates
                                     | otherwise   = pStates
  where
    (_, _, _, transitions, _) = iolts
    pStates                   = possibleStates transitions state
    isQuiescent               = quiescentCheck iolts state

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
straces' iolts []                   = []
straces' iolts ((state, path) : xs) = path : straces' iolts queue
  where
    (_, _, _, transitions, q) = iolts
    queue =
        xs
            ++ [ (snd x, path ++ [fst x])
               | x <- possibleSuspensionStates iolts state
               ]

straces :: IOLTS -> [Trace]
straces iolts = straces' iolts [(initState, [])]
    where (s, li, lu, transitions, initState) = pDelta iolts
