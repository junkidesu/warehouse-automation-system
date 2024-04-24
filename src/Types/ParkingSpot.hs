{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.ParkingSpot (ParkingSpot (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Car (Car)
import Types.ParkingLot (ParkingLot)

data ParkingSpot = ParkingSpot
    { car :: !Car
    , parkingLot :: !ParkingLot
    , latitude :: !Double
    , longitude :: !Double
    }
    deriving (Show, Generic)

instance FromJSON ParkingSpot
instance ToJSON ParkingSpot
instance ToSchema ParkingSpot
instance FromRow ParkingSpot where
    fromRow :: RowParser ParkingSpot
    fromRow =
        ParkingSpot
            <$> fromRow
            <*> fromRow
            <*> field
            <*> field
