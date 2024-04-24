{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api (WarehouseAPI, warehouseServer) where

import Api.ParkingLots (ParkingLotsAPI, parkingLotsServer)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant

type WarehouseAPI = ParkingLotsAPI

warehouseServer :: Pool Connection -> Server WarehouseAPI
warehouseServer conns = parkingLotsServer conns
