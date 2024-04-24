{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Api.Ping (Ping, ping) where

import Data.Text (Text)
import Servant

type Ping = Summary "Utility endpoint for healthcheck" :> "ping" :> Get '[PlainText] Text

ping :: Server Ping
ping = return "pong"
