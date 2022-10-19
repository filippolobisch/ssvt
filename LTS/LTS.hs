module LTS where

import           After_Out_Ioco_Refuses
import           Data.List
import           Generators
import           LTS_Types
import           Qtraces
import           Straces
import           Test.QuickCheck
import           Traces


-- ---------------------------------
-- Main
-- ---------------------------------
ltsMain :: IO ()
ltsMain = do
    putStrLn
        (  "coffeeModel1 after [\"coin\"]: "
        ++ show (coffeeModel1 `after` ["coin"])
        )
    putStrLn
        ("coffeeModel1 out [\"coin\"]: " ++ show (coffeeModel1 `out` ["coin"]))
    putStrLn
        (  "coffeeImpl1 ioco coffeeModel1: "
        ++ show (coffeeImpl1 `ioco` coffeeModel1)
        )
    putStrLn
        ("coffeeImpl1 after \"coin\" refuses \"coin\": " ++ show
            (refuses coffeeImplSimple (coffeeImpl1 `after` ["coin"]) ["coin"])
        )

    putStrLn "\ntraces coffeeModel1:"
    print $ take 20 $ traces coffeeModel1
    putStrLn "\nqtraces coffeeModel1:"
    print $ take 20 $ qtraces coffeeModel1
    putStrLn "\nstraces coffeeModel1:"
    print $ take 20 $ straces coffeeModel1

    putStrLn
        (  "\nComparing Simon's coffeeModel1 straces with my straces function: "
        ++ show
               (  simonResult
               == take (length simonResult) (straces coffeeModel1)
               )
        )

    -- verboseCheck $ forAll ltsGenerator propLTSGenTest
    -- verboseCheck $ forAll ioltsGenerator propIOLTSGenTest
