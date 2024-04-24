{-# LANGUAGE DeriveGeneric #-}

module Types.Shipment.Feedback (Feedback (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import Database.PostgreSQL.Simple.FromRow (FromRow (fromRow), field)
import GHC.Generics (Generic)
import Types.Car (Car)

data Feedback = Feedback
    { car :: Car
    , isSatisfied :: !Bool
    , message :: !(Maybe Text)
    }
    deriving (Show, Generic)

instance FromJSON Feedback
instance ToJSON Feedback
instance ToSchema Feedback
instance FromRow Feedback where
    fromRow =
        Feedback
            <$> fromRow
            <*> field
            <*> field
