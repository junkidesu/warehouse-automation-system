{-# LANGUAGE DeriveGeneric #-}

module Types.ParkingLot (ParkingLot (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data ParkingLot = ParkingLot
    { id :: !Int
    , latitude :: !Double
    , longitude :: !Double
    , city :: !Text
    }
    deriving (Show, Generic)

instance FromRow ParkingLot
instance ToRow ParkingLot
instance FromJSON ParkingLot
instance ToJSON ParkingLot
instance ToSchema ParkingLot
