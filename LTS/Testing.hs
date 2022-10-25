module Testing where

import           After_Refuses
import           Data.List
import           Generators
import           LTS_Types
import           Qtraces
import           Straces
import           Test.QuickCheck
import           Traces


-- Property that checks whether a passed IOLTS model/impl is in ioco with itself.
-- The function should return true, otherwise there is a fault with the ioco method.
-- It should return true because every transition in the iolts should be visible in both sides of the `ioco` method.
-- It does not provide much information about the `ioco` and IOLTS method as it only checks the IOLTS against itself.
propertyIocoEqualSelf :: IOLTS -> Bool
propertyIocoEqualSelf iolts = iolts `ioco` iolts

-- Property that checks whether the first element of the traces method of a passed IOLTS model/impl is an empty array.
propertyFirstElementTracesIsEmptyTrace :: IOLTS -> Bool
propertyFirstElementTracesIsEmptyTrace iolts = null (head (traces iolts))

-- Property that checks whether the labels in a given trace and the labels of the trace are part of the input and output labels
-- of the IOLTS model.
propertyTraceIsInLabels :: IOLTS -> Bool
propertyTraceIsInLabels iolts = and
    [ label `elem` (li `union` lu) | trace <- traces iolts, label <- trace ]
    where (_, li, lu, _, _) = iolts
