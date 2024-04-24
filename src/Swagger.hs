{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Swagger (API, server) where

import Api
import Data.Pool (Pool)
import Data.Swagger (Swagger)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Swagger
import Servant.Swagger.UI (SwaggerSchemaUI, swaggerSchemaUIServer)

type API =
    WarehouseAPI
        :<|> SwaggerSchemaUI "swagger-ui" "swagger.json"

swaggerDoc :: Swagger
swaggerDoc = toSwagger (Proxy :: Proxy WarehouseAPI)

server :: Pool Connection -> Server API
server conns =
    warehouseServer conns
        :<|> swaggerSchemaUIServer swaggerDoc
