{-# LANGUAGE DeriveGeneric #-}

module Types.ParkingSpot.New (NewParkingSpot (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data NewParkingSpot = NewParkingSpot
    { carId :: !Text
    , manufacturer :: !(Maybe Text)
    , model :: !Text
    , color :: !Text
    , latitude :: Double
    , longitude :: Double
    }
    deriving (Show, Generic)

instance FromJSON NewParkingSpot
instance ToJSON NewParkingSpot
instance ToSchema NewParkingSpot
