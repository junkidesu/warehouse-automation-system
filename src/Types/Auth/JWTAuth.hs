{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.Auth.JWTAuth (JWTAuth) where

import Servant.Auth
import qualified Types.Auth.User as AU

type JWTAuth = Auth '[JWT] AU.AuthUser
