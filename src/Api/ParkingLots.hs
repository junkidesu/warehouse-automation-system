{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.ParkingLots (ParkingLotsAPI, parkingLotsServer) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Pool (Pool)
import Data.Text (Text)
import Database.Operations (allParkingLots, deleteParkedCar, insertParkedCar, insertParkingLot, parkedCarById, parkedCars, parkingLotById)
import Database.PostgreSQL.Simple (Connection)
import Servant
import Servant.Auth.Server (AuthResult (Authenticated), ThrowAll (throwAll))
import Types.Auth.JWTAuth (JWTAuth)
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)
import Types.ParkingSpot (ParkingSpot)
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

type GetParkedCarById =
    Summary "Get parked car by ID"
        :> Capture "car_id" Text
        :> Get '[JSON] ParkingSpot

type UnparkCar =
    Summary "Remove a car from the parking lot"
        :> Capture "car_id" Text
        :> Verb 'DELETE 204 '[JSON] NoContent

type ParkedCarsAPI =
    Capture "id" Int
        :> "cars"
        :> (ParkCar :<|> GetParkedCars :<|> GetParkedCarById :<|> UnparkCar)

type ParkingLotsAPI =
    "parking"
        :> JWTAuth
        :> ( GetAllParkingLots
                :<|> AddParkingLot
                :<|> GetParkingLotById
                :<|> ParkedCarsAPI
           )

parkingLotsServer :: Pool Connection -> Server ParkingLotsAPI
parkingLotsServer conns (Authenticated _) =
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
    parkedCarsServer plId =
        parkCar
            :<|> getParkedCars
            :<|> getParkedCarById
            :<|> unparkCar
      where
        parkCar :: NPS.NewParkingSpot -> Handler PS.ParkingSpot
        parkCar nps = liftIO $ insertParkedCar conns plId nps

        getParkedCars :: Handler [PS.ParkingSpot]
        getParkedCars = liftIO $ parkedCars conns plId

        getParkedCarById :: Text -> Handler PS.ParkingSpot
        getParkedCarById carId = do
            mbParkingSpot <- liftIO $ parkedCarById conns plId carId

            case mbParkingSpot of
                Nothing -> throwError err404
                Just parkingSpot -> return parkingSpot

        unparkCar :: Text -> Handler NoContent
        unparkCar carId = liftIO $ deleteParkedCar conns plId carId >> pure NoContent
parkingLotsServer _ _ = throwAll err401
