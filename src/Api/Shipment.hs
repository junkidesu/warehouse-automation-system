{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Shipment (ShipmentAPI, shipmentServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Data.Text (Text)
import Database (ensureExists, ensureExistsReturning)
import Database.Operations (allShipments, deleteParkedCar, insertShipment, parkedCarById, shipmentById, updateShipmentState)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Types.Auth.JWTAuth (JWTAuth)
import qualified Types.ParkingLot as PL
import qualified Types.ParkingSpot as PS
import qualified Types.Shipment as S
import Types.Shipment.Action (Action (..))
import qualified Types.Shipment.Action as AB
import qualified Types.Shipment.New as NS
import Types.Shipment.State (State (..))

type GetAllShipments =
  Summary "Get all shipments"
    :> Get '[JSON] [S.Shipment]

type PostShipment =
  Summary "Post a new shipment"
    :> ReqBody '[JSON] NS.NewShipment
    :> PostCreated '[JSON] S.Shipment

type ChangeState =
  Summary "Change the state of the shipment"
    :> Capture "car_id" Text
    :> "state"
    :> ReqBody '[JSON] AB.ActionBody
    :> Post '[JSON] NoContent

type Protected =
  JWTAuth
    :> ( GetAllShipments
          :<|> PostShipment
          :<|> ChangeState
       )

type ShipmentAPI = "shipment" :> Protected

shipmentServer :: Pool Connection -> Server ShipmentAPI
shipmentServer conns = protectedServer
 where
  protectedServer :: Server Protected
  protectedServer (Authenticated _) =
    getAllShipments
      :<|> postShipment
      :<|> changeState
   where
    getAllShipments :: Handler [S.Shipment]
    getAllShipments = liftIO $ allShipments conns

    postShipment :: NS.NewShipment -> Handler S.Shipment
    postShipment ns = do
      ensureExists conns parkedCarById (NS.carId ns)

      liftIO $ insertShipment conns ns

    changeState :: Text -> AB.ActionBody -> Handler NoContent
    changeState carId ab = do
      shipment <- ensureExistsReturning conns shipmentById carId
      let currentState = S.state shipment
          action = AB.action ab
      helper action currentState
     where
      helper :: Action -> State -> Handler NoContent
      helper StartDelivery OnParking = do
        parkedCar <- ensureExistsReturning conns parkedCarById carId

        liftIO $ do
          deleteParkedCar conns (PL.id . PS.parkingLot $ parkedCar) carId
          updateShipmentState conns carId DeliveryStarted

        return NoContent
      helper CompleteDelivery DeliveryStarted = do
        liftIO $ updateShipmentState conns carId DeliveryComplete
        return NoContent
      helper Receive _ = do
        liftIO $ updateShipmentState conns carId Received
        return NoContent
      helper _ _ = throwError err400
  protectedServer _ = throwAll err401
