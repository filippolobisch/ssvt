module OrderOfRelations where

import           Data.List
import           Data.Tuple
import           PropertiesOfRelations
import           Relations


preOrder r = reflexive r && transitive r

quasiOrder r = reflexive r && transitive r

strictPreOrder r = irreflexive r && transitive r

nonStrictPartialOrder r = reflexive r && asymmetric r && transitive r

strictPartialOrder r =
    irreflexive r && asymmetric r && antisymmetric r && transitive r

partialOrder r = reflexive r && antisymmetric r && transitive r

totalOrder r = reflexive r && antisymmetric r && transitive r

equivalence r = reflexive r && symmetric r && transitive r
