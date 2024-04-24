{-# LANGUAGE OverloadedStrings #-}

module Database.Queries (
    allParkingLotsQuery,
    insertParkingLotQuery,
    parkingLotByIdQuery,
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
