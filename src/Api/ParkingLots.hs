{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.ParkingLots (ParkingLotsAPI, parkingLotsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Database.Operations (allParkingLots, insertParkedCar, insertParkingLot, parkedCars, parkingLotById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)
import qualified Types.ParkingSpot as PS
import qualified Types.ParkingSpot.New as NPS

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

type ParkCar =
    Summary "Park a car in the parking lot"
        :> ReqBody '[JSON] NPS.NewParkingSpot
        :> PostCreated '[JSON] PS.ParkingSpot

type GetParkedCars =
    Summary "Get cars parked in the parking lot"
        :> Get '[JSON] [PS.ParkingSpot]

type ParkedCarsAPI =
    Capture "id" Int
        :> "cars"
        :> (ParkCar :<|> GetParkedCars)

type ParkingLotsAPI =
    "parking"
        :> ( GetAllParkingLots
                :<|> AddParkingLot
                :<|> GetParkingLotById
                :<|> ParkedCarsAPI
           )

parkingLotsServer :: Pool Connection -> Server ParkingLotsAPI
parkingLotsServer conns =
    getAllParkingLots
        :<|> addParkingLot
        :<|> getParkingLotById
        :<|> parkedCarsServer
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

    parkedCarsServer :: Server ParkedCarsAPI
    parkedCarsServer plId = parkCar :<|> getParkedCars
      where
        parkCar :: NPS.NewParkingSpot -> Handler PS.ParkingSpot
        parkCar nps = liftIO $ insertParkedCar conns plId nps

        getParkedCars :: Handler [PS.ParkingSpot]
        getParkedCars = liftIO $ parkedCars conns plId
