module Database.Operations (
    allParkingLots,
    insertParkingLot,
    parkingLotById,
) where

import Data.Pool (Pool)
import Database (getMany_, getOne, insertReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries (allParkingLotsQuery, insertParkingLotQuery, parkingLotByIdQuery)
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)

allParkingLots :: Pool Connection -> IO [ParkingLot]
allParkingLots conns = getMany_ conns allParkingLotsQuery

insertParkingLot :: Pool Connection -> NewParkingLot -> IO ParkingLot
insertParkingLot conns =
    insertReturning
        conns
        insertParkingLotQuery

parkingLotById :: Pool Connection -> Int -> IO (Maybe ParkingLot)
parkingLotById conns plId = getOne conns parkingLotByIdQuery (Only plId)
