{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Shipment (ShipmentAPI, shipmentServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database (ensureExists)
import Database.Operations (allShipments, insertShipment, parkedCarById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.Shipment as S
import qualified Types.Shipment.New as NS

type GetAllShipments =
  Summary "Get all shipments"
    :> Get '[JSON] [S.Shipment]

type PostShipment =
  Summary "Post a new shipment"
    :> ReqBody '[JSON] NS.NewShipment
    :> PostCreated '[JSON] S.Shipment

type Protected =
  JWTAuth
    :> ( GetAllShipments
          :<|> PostShipment
       )

type ShipmentAPI = "shipment" :> Protected

shipmentServer :: Pool Connection -> Server ShipmentAPI
shipmentServer conns = protectedServer
 where
  protectedServer :: Server Protected
  protectedServer (Authenticated _) =
    getAllShipments
      :<|> postShipment
   where
    getAllShipments :: Handler [S.Shipment]
    getAllShipments = liftIO $ allShipments conns

    postShipment :: NS.NewShipment -> Handler S.Shipment
    postShipment ns = do
      ensureExists conns parkedCarById (NS.carId ns)

      liftIO $ insertShipment conns ns
  protectedServer _ = throwAll err401
