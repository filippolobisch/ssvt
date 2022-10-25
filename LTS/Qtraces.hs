module Qtraces where

import           After_Refuses
import           Data.List
import           LTS_Types
import           Test.QuickCheck
import           Traces

-- ---------------------------------
-- Qtraces
-- ---------------------------------
-- We use this method to check whether a state is a suspension state (also known as Quiescent in Tretman's paper).
-- A suspension state is determined if there are no output actions that can be performed by the system and therefore,
-- requires user input to continue.
suspensionStateCheck :: IOLTS -> State -> Bool
suspensionStateCheck (_, _, lu, transitions, _) state = all
    (\(f, l, t) -> f == state --> l `notElem` (lu `union` [tau]))
    transitions

suspensionStateCheck' :: IOLTS -> State -> Bool
suspensionStateCheck' (_, _, lu, transitions, _) state = and
    [ l `notElem` (lu `union` [tau]) | (f, l, t) <- transitions, f == state ]

-- Calculates all the traces that may lead to a suspension state.
-- Uses our previously defined after method and then checks whether at the returned state(s) there is an output action or not.
qtraces :: IOLTS -> [Trace]
qtraces iolts =
    [ trace
    | trace <- traces iolts
    , p1    <- iolts `after` trace
    , suspensionStateCheck iolts p1
    ]
