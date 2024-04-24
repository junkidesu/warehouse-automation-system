{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (WarehouseAPI, warehouseServer) where

import Api.Auth (AuthAPI, authServer)
import Api.ParkingLots (ParkingLotsAPI, parkingLotsServer)
import Api.Ping (Ping, ping)
import Api.Shipment (ShipmentAPI, shipmentServer)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (JWTSettings)

type WarehouseAPI =
    AuthAPI
        :<|> ParkingLotsAPI
        :<|> ShipmentAPI
        :<|> Ping

warehouseServer :: Pool Connection -> JWTSettings -> Server WarehouseAPI
warehouseServer conns jwts =
    authServer conns jwts
        :<|> parkingLotsServer conns
        :<|> shipmentServer conns
        :<|> ping
