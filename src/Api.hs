{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Servant

type WarehouseAPI = EmptyAPI

warehouseServer :: Pool Connection -> Server WarehouseAPI
warehouseServer _ = emptyServer
