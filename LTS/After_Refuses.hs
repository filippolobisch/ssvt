module After_Refuses where

import           Data.List
import           LTS_Types
import           Test.QuickCheck
import           Traces


-- ---------------------------------
-- After
-- ---------------------------------

-- Second argument is a first-in first-out queue of all the possible states, with the current Transition and the path taken.
-- The path is appended to the output and then its recursively called for the new possibleStates and path.
-- is = afterPStates input state, apl = afterPStates label, apos = afterPStates output state
after' :: [LabeledTransition] -> Trace -> [(State, Trace)] -> [State]
after' transitions trace [] = []
after' transitions trace ((state, path) : xs) =
    let pStates = possibleStates transitions state
        queue =
            xs
                ++ [ (t, path ++ [l])
                   | (l, t) <- pStates
                   , filter (/= tau) (path ++ [l]) `isInfixOf` trace
                   ]
    in  [ state | trace == filter (/= tau) path ]
        ++ after' transitions trace queue

infix 1 `after`
after :: IOLTS -> Trace -> [State]
after (_, _, _, ts, initState) trace = sort $ after' ts trace [(initState, [])]


-- ---------------------------------
-- Refuses
-- ---------------------------------
refuseCheck :: [LabeledTransition] -> State -> Label -> Bool
refuseCheck transitions s l =
    not (any (\(f, tl, t) -> f == s && tl == l) transitions)

refuses :: LTS -> [State] -> [Label] -> Bool
refuses (_, _, transitions, _) states labels =
    all (\s -> all (refuseCheck transitions s) (labels ++ [tau])) states
