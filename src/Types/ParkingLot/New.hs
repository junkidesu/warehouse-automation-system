{-# LANGUAGE DeriveGeneric #-}

module Types.ParkingLot.New (NewParkingLot (..)) where

import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (ToRow)
import GHC.Generics (Generic)

data NewParkingLot = NewParkingLot
    { latitude :: !Double
    , longitude :: !Double
    , city :: !Text
    }
    deriving (Show, Generic)

instance FromJSON NewParkingLot
instance ToRow NewParkingLot
instance ToSchema NewParkingLot
