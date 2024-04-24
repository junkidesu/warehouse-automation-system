{-# LANGUAGE DeriveGeneric #-}

module Types.User (User (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Database.PostgreSQL.Simple (FromRow)
import GHC.Generics (Generic)

data User = User
    { id :: !Int
    , username :: !Text
    , passwordHash :: !Text
    }
    deriving (Show, Generic)

instance FromJSON User
instance ToJSON User
instance FromRow User
