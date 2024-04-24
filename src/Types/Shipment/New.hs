{-# LANGUAGE DeriveGeneric #-}

module Types.Shipment.New (NewShipment (..)) where

import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)
import Types.Shipment.Mode (Mode)

data NewShipment = NewShipment
    { carId :: !Text
    , destination :: !Text
    , shipmentMode :: !Mode
    }
    deriving (Show, Generic)

instance FromJSON NewShipment
instance ToSchema NewShipment
