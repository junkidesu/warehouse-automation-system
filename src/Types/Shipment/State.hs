{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Shipment.State (State (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data State = OnParking | DeliveryStarted | DeliveryComplete | Received
    deriving (Show, Eq, Generic)

instance FromJSON State
instance ToJSON State
instance ToSchema State

instance ToField State where
    toField :: State -> Action
    toField OnParking = Escape "on_parking"
    toField DeliveryStarted = Escape "delivery_started"
    toField DeliveryComplete = Escape "delivery_complete"
    toField Received = Escape "received"

instance FromField State where
    fromField field mdata =
        if typeOid field /= typoid text
            then returnError Incompatible field ""
            else case unpack <$> mdata of
                Nothing -> returnError UnexpectedNull field ""
                Just dat -> case dat of
                    "on_parking" -> pure OnParking
                    "delivery_started" -> pure DeliveryStarted
                    "delivery_complete" -> pure DeliveryComplete
                    "received" -> pure Received
                    _ -> returnError ConversionFailed field dat
