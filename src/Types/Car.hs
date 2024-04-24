{-# LANGUAGE DeriveGeneric #-}

module Types.Car (Car (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Swagger (ToSchema)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow, ToRow)
import GHC.Generics (Generic)

data Car = Car
    { id :: !Text
    , manufacturer :: !(Maybe Text)
    , model :: !Text
    , color :: !Text
    }
    deriving (Show, Generic)

instance FromJSON Car
instance ToJSON Car
instance ToSchema Car
instance FromRow Car
instance ToRow Car
