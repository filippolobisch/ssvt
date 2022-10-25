module Straces where

import           After_Refuses
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
    (\s -> if suspensionStateCheck iolts s
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
straces iolts = traces (pDelta iolts)

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

-- ---------------------------------
-- Out and IOCO
-- ---------------------------------

-- Out method that follows tretmans definition.
-- 1. out(q) =def { x ∈ LU |q −→ } ∪ {δ|δ(q)}
infix 1 `out`
out :: IOLTS -> State -> [Label]
out iolts state =
    nub
        $       [ l | (f, l, t) <- transitions, state == f, l `elem` lu ]
        `union` [ delta | suspensionStateCheck iolts state ]
    where (_, _, lu, transitions, _) = iolts


-- Out that is compatible with a IOLTS with delta already included.
infix 1 `out'`
out' :: IOLTS -> State -> [Label]
out' (_, _, lu, transitions, _) state =
    nub [ l | (f, l, t) <- transitions, state == f, l `elem` lu ]

-- method that performs the after and out functions in 'one' go per se.
-- Helpful to avoid repeating that process multiple times in the ioco method.
infix 1 `outAfter`
outAfter :: IOLTS -> Trace -> [Label]
outAfter iolts trace = nub $ concatMap (out iolts) $ iolts `after` trace

-- method that performs the after and out functions in 'one' go per se.
-- Helpful to avoid repeating that process multiple times in the ioco method.
-- This one use the out method that is compatible with an iolts that has delta already included.
infix 1 `outAfterDelta`
outAfterDelta :: IOLTS -> Trace -> [Label]
outAfterDelta iolts trace = nub $ concatMap (out' iolts) $ iolts `after` trace

-- IOCO method that checks whether two IOLTS methods are in an ioco relationship.
-- Follows the exact definition of the tretman paper.
-- Has potential to run forever.
infix 1 `ioco`
ioco :: IOLTS -> IOLTS -> Bool
ioco implementation model = and
    [ l `elem` outAfter model trace
    | trace <- straces model
    , l     <- outAfter implementation trace
    ]

-- IOCO method that has the IOLTS implementation and model that include the 'delta' symbol in the transitions.
-- Uses the outAfterDelta method which does not append delta to the output labels if it is a suspension state as this method already takes care of it.
infix 1 `iocoDelta`
iocoDelta :: IOLTS -> IOLTS -> Bool
iocoDelta implementation model = and
    [ l `elem` outAfterDelta dModel trace
    | trace <- straces model
    , l     <- outAfterDelta dImpl trace
    ]
  where
    dImpl  = pDelta implementation
    dModel = pDelta model


-- IOCO methods that are restricted to 100 straces of the model.
-- infix 1 `ioco`
-- ioco :: IOLTS -> IOLTS -> Bool
-- ioco implementation model =
--     all
--             (\trace -> all (`elem` outAfter model trace)
--                            (outAfter implementation trace)
--             )
--         $ take 100
--         $ straces model

-- infix 1 `iocoDelta`
-- iocoDelta :: IOLTS -> IOLTS -> Bool
-- iocoDelta implementation model =
--     all (\trace -> all (`elem` outAfter dModel trace) (outAfter dImpl trace))
--         $ take 100
--         $ straces model
--   where
--     dImpl  = pDelta implementation
--     dModel = pDelta model
