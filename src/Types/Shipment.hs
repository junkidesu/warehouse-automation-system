{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}

module Types.Shipment (Shipment (..)) where

import Data.Aeson (ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromRow
import GHC.Generics (Generic)
import Types.Car (Car)
import Types.Shipment.Mode (Mode)
import Types.Shipment.State (State)

data Shipment = Shipment
    { car :: !Car
    , destination :: !Text
    , mode :: Mode
    , state :: State
    }
    deriving (Show, Generic)

instance ToJSON Shipment
instance FromRow Shipment where
    fromRow :: RowParser Shipment
    fromRow =
        Shipment
            <$> fromRow
            <*> field
            <*> field
            <*> field

instance ToSchema Shipment
