{-# LANGUAGE DeriveGeneric #-}

module Types.Shipment.Action (Action (..), ActionBody (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)

data Action = StartDelivery | CompleteDelivery | Receive
    deriving (Show, Generic)

instance FromJSON Action
instance ToJSON Action
instance ToSchema Action

data ActionBody = ActionBody
    { action :: !Action
    }
    deriving (Show, Generic)

instance FromJSON ActionBody
instance ToSchema ActionBody
