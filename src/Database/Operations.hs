module Database.Operations (
    allParkingLots,
    insertParkingLot,
    parkingLotById,
    parkedCars,
    insertParkedCar,
    deleteParkedCar,
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (delete, getMany, getMany_, getOne, insertReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries (allParkingLotsQuery, deleteParkedCarQuery, insertParkedCarQuery, insertParkingLotQuery, parkedCarsQuery, parkingLotByIdQuery)
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)
import Types.ParkingSpot (ParkingSpot)
import qualified Types.ParkingSpot.New as NPS

allParkingLots :: Pool Connection -> IO [ParkingLot]
allParkingLots conns = getMany_ conns allParkingLotsQuery

insertParkingLot :: Pool Connection -> NewParkingLot -> IO ParkingLot
insertParkingLot conns =
    insertReturning
        conns
        insertParkingLotQuery

parkingLotById :: Pool Connection -> Int -> IO (Maybe ParkingLot)
parkingLotById conns plId = getOne conns parkingLotByIdQuery (Only plId)

parkedCars :: Pool Connection -> Int -> IO [ParkingSpot]
parkedCars conns plId = getMany conns parkedCarsQuery (Only plId)

insertParkedCar :: Pool Connection -> Int -> NPS.NewParkingSpot -> IO ParkingSpot
insertParkedCar conns plId nps =
    insertReturning
        conns
        insertParkedCarQuery
        (NPS.carId nps, NPS.manufacturer nps, NPS.model nps, NPS.color nps, plId, NPS.latitude nps, NPS.longitude nps)

deleteParkedCar :: Pool Connection -> Int -> Text -> IO ()
deleteParkedCar conns plId carId = delete conns deleteParkedCarQuery (plId, carId)
