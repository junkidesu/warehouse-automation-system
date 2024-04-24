{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.ParkingLots (ParkingLotsAPI, parkingLotsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations (allParkingLots, insertParkingLot, parkingLotById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)

type GetAllParkingLots =
    Summary "Get all parking lots"
        :> Get '[JSON] [ParkingLot]

type AddParkingLot =
    Summary "Add a parking lot"
        :> ReqBody '[JSON] NewParkingLot
        :> PostCreated '[JSON] ParkingLot

type GetParkingLotById =
    Summary "Get a specific parking lot by ID"
        :> Capture "id" Int
        :> Get '[JSON] ParkingLot

type ParkingLotsAPI =
    "parking"
        :> ( GetAllParkingLots
                :<|> AddParkingLot
                :<|> GetParkingLotById
           )

parkingLotsServer :: Pool Connection -> Server ParkingLotsAPI
parkingLotsServer conns = getAllParkingLots :<|> addParkingLot :<|> getParkingLotById
  where
    getAllParkingLots :: Handler [ParkingLot]
    getAllParkingLots = liftIO $ allParkingLots conns

    addParkingLot :: NewParkingLot -> Handler ParkingLot
    addParkingLot = liftIO . insertParkingLot conns

    getParkingLotById :: Int -> Handler ParkingLot
    getParkingLotById plId = do
        mbParkingLot <- liftIO $ parkingLotById conns plId

        case mbParkingLot of
            Nothing -> throwError err404
            Just parkingLot -> return parkingLot
