{-# LANGUAGE DeriveGeneric #-}

module Types.Shipment.Feedback.New (NewFeedback (..)) where

import Data.Aeson (FromJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import GHC.Generics (Generic)

data NewFeedback = NewFeedback
    { isSatisfied :: !Bool
    , message :: !Text
    }
    deriving (Show, Generic)

instance FromJSON NewFeedback
instance ToSchema NewFeedback
