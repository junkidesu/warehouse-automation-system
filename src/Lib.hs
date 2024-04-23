module Lib (
    someFunc,
) where

data Slot = Slot
    { lat :: !Double
    , long :: !Double
    }

newtype ParkingLot = ParkingLot
    { slots :: [Slot]
    }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
