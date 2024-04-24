{-# LANGUAGE OverloadedStrings #-}

module Database.Queries (
    allParkingLotsQuery,
    insertParkingLotQuery,
    parkingLotByIdQuery,
    insertParkedCarQuery,
    parkedCarsQuery,
    parkedCarByIdQuery,
    deleteParkedCarQuery,
) where

import Database (toSqlQuery)
import Database.PostgreSQL.Simple (Query)

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
        , "WHERE p.id = ? AND c.id = ?"
        ]

deleteParkedCarQuery :: Query
deleteParkedCarQuery =
    toSqlQuery
        [ "DELETE FROM parking_spots"
        , "WHERE parking_lot_id = ? AND car_id = ?"
        ]
