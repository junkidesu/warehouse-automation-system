{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Swagger (API, server) where

import Api
import Api.Auth
import Api.ParkingLots
import Api.Ping
import Api.Shipment
import Control.Lens
import Data.Pool (Pool)
import Data.Swagger
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (JWTSettings)
import Servant.Auth.Swagger ()
import Servant.Swagger
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API =
    WarehouseAPI
        :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

authOpts :: Traversal' Swagger Operation
authOpts = subOperations (Proxy :: Proxy AuthAPI) (Proxy :: Proxy WarehouseAPI)

parkingLotsOpts :: Traversal' Swagger Operation
parkingLotsOpts = subOperations (Proxy :: Proxy ParkingLotsAPI) (Proxy :: Proxy WarehouseAPI)

shipmentOpts :: Traversal' Swagger Operation
shipmentOpts = subOperations (Proxy :: Proxy ShipmentAPI) (Proxy :: Proxy WarehouseAPI)

otherOpts :: Traversal' Swagger Operation
otherOpts = subOperations (Proxy :: Proxy Ping) (Proxy :: Proxy WarehouseAPI)

swaggerDoc :: Swagger
swaggerDoc =
    toSwagger (Proxy :: Proxy WarehouseAPI)
        & info . title .~ "Warehouse Automation System API"
        & info . version .~ "0.1.0.0"
        & info . description ?~ "Terrabyte Hackathon 2024. TTPU"
        & info . license ?~ "BSD"
        & applyTagsFor authOpts ["authentication" & description ?~ "Authenticate to the system"]
        & applyTagsFor parkingLotsOpts ["parking" & description ?~ "Manage parking lots"]
        & applyTagsFor shipmentOpts ["shipment" & description ?~ "Manage shipment"]
        & applyTagsFor otherOpts ["other" & description ?~ "Utility and test endpoints"]

server :: Pool Connection -> JWTSettings -> Server API
server conns jwts =
    warehouseServer conns jwts
        :<|> swaggerSchemaUIServer swaggerDoc
