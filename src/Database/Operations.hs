module Database.Operations (
    userByUsername,
    allParkingLots,
    insertParkingLot,
    parkingLotById,
    parkedCars,
    parkedCarById,
    parkedCarByIdAndParkingLot,
    insertParkedCar,
    deleteParkedCar,
    allShipments,
    shipmentById,
    insertShipment,
    updateShipmentState,
    feedbackById,
    insertFeedback,
) where

import Data.Pool (Pool)
import Data.Text (Text)
import Database (delete, getMany, getMany_, getOne, insert, insertReturning)
import Database.PostgreSQL.Simple (Connection, Only (Only))
import Database.Queries
import Types.ParkingLot (ParkingLot)
import Types.ParkingLot.New (NewParkingLot)
import Types.ParkingSpot (ParkingSpot)
import qualified Types.ParkingSpot.New as NPS
import Types.Shipment (Shipment)
import Types.Shipment.Feedback (Feedback)
import qualified Types.Shipment.Feedback.New as NF
import qualified Types.Shipment.New as NS
import Types.Shipment.State (State)
import Types.User (User)

userByUsername :: Pool Connection -> Text -> IO (Maybe User)
userByUsername conns username = getOne conns userByUsernameQuery (Only username)

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

parkedCarByIdAndParkingLot :: Pool Connection -> Int -> Text -> IO (Maybe ParkingSpot)
parkedCarByIdAndParkingLot conns plId carId =
    getOne
        conns
        parkedCarByIdAndParkingLotQuery
        (plId, carId)

parkedCarById :: Pool Connection -> Text -> IO (Maybe ParkingSpot)
parkedCarById conns carId =
    getOne
        conns
        parkedCarByIdQuery
        (Only carId)

insertParkedCar :: Pool Connection -> Int -> NPS.NewParkingSpot -> IO ParkingSpot
insertParkedCar conns plId nps =
    insertReturning
        conns
        insertParkedCarQuery
        (NPS.carId nps, NPS.manufacturer nps, NPS.model nps, NPS.color nps, plId, NPS.latitude nps, NPS.longitude nps)

deleteParkedCar :: Pool Connection -> Int -> Text -> IO ()
deleteParkedCar conns plId carId = delete conns deleteParkedCarQuery (plId, carId)

allShipments :: Pool Connection -> IO [Shipment]
allShipments conns = getMany_ conns allShipmentsQuery

shipmentById :: Pool Connection -> Text -> IO (Maybe Shipment)
shipmentById conns carId = getOne conns shipmentByIdQuery (Only carId)

insertShipment :: Pool Connection -> NS.NewShipment -> IO Shipment
insertShipment conns newShipment =
    insertReturning
        conns
        insertShipmentQuery
        (NS.carId newShipment, NS.destination newShipment, NS.shipmentMode newShipment)

updateShipmentState :: Pool Connection -> Text -> State -> IO ()
updateShipmentState conns carId newState = insert conns updateShipmentStateQuery (newState, carId)

feedbackById :: Pool Connection -> Text -> IO (Maybe Feedback)
feedbackById conns carId = getOne conns feedbackByIdQuery (Only carId)

insertFeedback :: Pool Connection -> Text -> NF.NewFeedback -> IO Feedback
insertFeedback conns carId nf =
    insertReturning
        conns
        insertFeedbackQuery
        (carId, NF.isSatisfied nf, NF.message nf)
