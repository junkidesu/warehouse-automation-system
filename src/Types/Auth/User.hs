{-# LANGUAGE DeriveGeneric #-}

module Types.Auth.User (AuthUser (..)) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.Auth.JWT (FromJWT, ToJWT)

data AuthUser = AuthUser
    { id :: !Int
    , username :: !Text
    }
    deriving (Show, Read, Generic)

instance ToJSON AuthUser
instance FromJSON AuthUser
instance ToJWT AuthUser
instance FromJWT AuthUser
