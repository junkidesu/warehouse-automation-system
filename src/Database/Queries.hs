{-# LANGUAGE OverloadedStrings #-}

module Database.Queries (
    userByUsernameQuery,
    allParkingLotsQuery,
    insertParkingLotQuery,
    parkingLotByIdQuery,
    insertParkedCarQuery,
    parkedCarsQuery,
    parkedCarByIdAndParkingLotQuery,
    parkedCarByIdQuery,
    deleteParkedCarQuery,
    allShipmentsQuery,
    shipmentByIdQuery,
    insertShipmentQuery,
    updateShipmentStateQuery,
    feedbackByIdQuery,
    insertFeedbackQuery,
) where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

userByUsernameQuery :: Query
userByUsernameQuery =
    toSqlQuery
        [ "SELECT * FROM users"
        , "WHERE username = ?"
        ]

allParkingLotsQuery :: Query
allParkingLotsQuery =
    toSqlQuery
        [ "SELECT * FROM parking_lots"
        ]

insertParkingLotQuery :: Query
insertParkingLotQuery =
    toSqlQuery
        [ "INSERT INTO parking_lots (latitude, longitude, city)"
        , "VALUES (?, ?, ?)"
        , "RETURNING *"
        ]

parkingLotByIdQuery :: Query
parkingLotByIdQuery =
    toSqlQuery
        [ "SELECT * FROM parking_lots"
        , "WHERE id = ?"
        ]

insertParkedCarQuery :: Query
insertParkedCarQuery =
    toSqlQuery
        [ "WITH inserted_car AS ("
        , "INSERT INTO cars (id, manufacturer, model, color)"
        , "VALUES (?, ?, ?, ?)"
        , "RETURNING *),"
        , "inserted_spot AS ("
        , "INSERT INTO parking_spots (car_id, parking_lot_id, latitude, longitude)"
        , "VALUES ((SELECT id FROM inserted_car), ?, ?, ?)"
        , "RETURNING *)"
        , "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "p.id, p.latitude, p.longitude, p.city,"
        , "s.latitude, s.longitude"
        , "FROM inserted_car c"
        , "JOIN inserted_spot s"
        , "ON c.id = s.car_id"
        , "JOIN parking_lots p"
        , "ON p.id = s.parking_lot_id"
        ]

parkedCarsQuery :: Query
parkedCarsQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "p.id, p.latitude, p.longitude, p.city,"
        , "s.latitude, s.longitude"
        , "FROM cars c"
        , "JOIN parking_spots s"
        , "ON c.id = s.car_id"
        , "JOIN parking_lots p"
        , "ON p.id = s.parking_lot_id"
        , "WHERE p.id = ?"
        ]

parkedCarByIdAndParkingLotQuery :: Query
parkedCarByIdAndParkingLotQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "p.id, p.latitude, p.longitude, p.city,"
        , "s.latitude, s.longitude"
        , "FROM cars c"
        , "JOIN parking_spots s"
        , "ON c.id = s.car_id"
        , "JOIN parking_lots p"
        , "ON p.id = s.parking_lot_id"
        , "WHERE p.id = ? AND c.id = ?"
        ]

parkedCarByIdQuery :: Query
parkedCarByIdQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "p.id, p.latitude, p.longitude, p.city,"
        , "s.latitude, s.longitude"
        , "FROM cars c"
        , "JOIN parking_spots s"
        , "ON c.id = s.car_id"
        , "JOIN parking_lots p"
        , "ON p.id = s.parking_lot_id"
        , "WHERE c.id = ?"
        ]

deleteParkedCarQuery :: Query
deleteParkedCarQuery =
    toSqlQuery
        [ "DELETE FROM parking_spots"
        , "WHERE parking_lot_id = ? AND car_id = ?"
        ]

allShipmentsQuery :: Query
allShipmentsQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "s.destination, s.shipment_mode, s.shipment_state"
        , "FROM cars c"
        , "JOIN shipments s"
        , "ON s.car_id = c.id"
        ]

shipmentByIdQuery :: Query
shipmentByIdQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "s.destination, s.shipment_mode, s.shipment_state"
        , "FROM cars c"
        , "JOIN shipments s"
        , "ON s.car_id = c.id"
        , "WHERE c.id = ?"
        ]

insertShipmentQuery :: Query
insertShipmentQuery =
    toSqlQuery
        [ "WITH inserted_shipment AS ("
        , "INSERT INTO shipments (car_id, destination, shipment_mode, shipment_state)"
        , "VALUES (?, ?, ?, 'on_parking')"
        , "RETURNING *)"
        , "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "s.destination, s.shipment_mode, s.shipment_state"
        , "FROM inserted_shipment s"
        , "JOIN cars c"
        , "ON c.id = s.car_id"
        ]

updateShipmentStateQuery :: Query
updateShipmentStateQuery =
    toSqlQuery
        [ "UPDATE shipments"
        , "SET shipment_state = ?"
        , "WHERE car_id = ?"
        ]

feedbackByIdQuery :: Query
feedbackByIdQuery =
    toSqlQuery
        [ "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "f.is_satisfied, f.message"
        , "FROM feedbacks f"
        , "JOIN cars c"
        , "ON c.id = f.car_id"
        , "WHERE c.id = ?"
        ]

insertFeedbackQuery :: Query
insertFeedbackQuery =
    toSqlQuery
        [ "WITH inserted_feedback AS ("
        , "INSERT INTO feedbacks (car_id, is_satisfied, message)"
        , "VALUES (?, ?, ?)"
        , "RETURNING *)"
        , "SELECT"
        , "c.id, c.manufacturer, c.model, c.color,"
        , "f.is_satisfied, f.message"
        , "FROM inserted_feedback f"
        , "JOIN cars c"
        , "ON c.id = f.car_id"
        ]
