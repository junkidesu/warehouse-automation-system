{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Swagger (API, server) where

import Api
import Api.ParkingLots
import Control.Lens
import Data.Pool (Pool)
import Data.Swagger
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Swagger
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API =
    WarehouseAPI
        :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

parkingLotsOpts :: Traversal' Swagger Operation
parkingLotsOpts = subOperations (Proxy :: Proxy ParkingLotsAPI) (Proxy :: Proxy WarehouseAPI)

swaggerDoc :: Swagger
swaggerDoc =
    toSwagger (Proxy :: Proxy WarehouseAPI)
        & info . title .~ "Warehouse Automation System API"
        & info . version .~ "0.1.0.0"
        & info . description ?~ "Terrabyte Hackathon 2024. TTPU"
        & info . license ?~ "BSD"
        & applyTagsFor parkingLotsOpts ["parking lots" & description ?~ "Manage parking lots"]

server :: Pool Connection -> Server API
server conns =
    warehouseServer conns
        :<|> swaggerSchemaUIServer swaggerDoc