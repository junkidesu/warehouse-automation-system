{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Shipment.Mode (Mode (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 (unpack)
import Data.Swagger (ToSchema)
import Database.PostgreSQL.Simple.FromField (Field (typeOid), FromField (fromField), ResultError (ConversionFailed, Incompatible, UnexpectedNull), TypeInfo (typoid), returnError)
import Database.PostgreSQL.Simple.ToField (Action (Escape), ToField (toField))
import Database.PostgreSQL.Simple.TypeInfo.Static (text)
import GHC.Generics (Generic)

data Mode = Truck | Train | Self
    deriving (Show, Generic)

instance ToSchema Mode
instance FromJSON Mode
instance ToJSON Mode

instance ToField Mode where
    toField :: Mode -> Action
    toField Truck = Escape "truck"
    toField Train = Escape "train"
    toField Self = Escape "self"

instance FromField Mode where
    fromField field mdata =
        if typeOid field /= typoid text
            then returnError Incompatible field ""
            else case unpack <$> mdata of
                Nothing -> returnError UnexpectedNull field ""
                Just dat -> case dat of
                    "truck" -> pure Truck
                    "train" -> pure Train
                    "self" -> pure Self
                    _ -> returnError ConversionFailed field dat
