module Qtraces where

import           After_Out_Ioco_Refuses
import           Data.List
import           LTS_Types
import           Test.QuickCheck
import           Traces

-- ---------------------------------
-- Qtraces
-- ---------------------------------
quiescentCheck :: IOLTS -> State -> Bool
quiescentCheck (_, _, lu, transitions, _) state = all
    (\(f, l, t) -> f == state --> l `notElem` (lu `union` [tau]))
    transitions

qtraces :: IOLTS -> [Trace]
qtraces iolts =
    [ trace
    | trace <- traces iolts
    , p1    <- iolts `after` trace
    , quiescentCheck iolts p1
    ]
