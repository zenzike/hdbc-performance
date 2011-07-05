{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.PostgreSQL

import Data.Time.Calendar (Day, fromGregorian)
import Data.Time.LocalTime (TimeOfDay(TimeOfDay), LocalTime(LocalTime))

import Control.Monad (replicateM, forM, liftM)
import Debug.Trace

main :: IO ()
main = do
  -- Connect
  connPostgreSQL <- connectPostgreSQL "host=localhost dbname=hdbc-test user=hdbc password=qwerfdsa"
  connODBC       <- connectODBC "DSN=HDBC"

  -- Setup
  setupInsert connPostgreSQL
  setupSelect connPostgreSQL 10000

  -- Benchmark
  defaultMain
    [ benchBackend "odbc"       connODBC
    , benchBackend "postgresql" connPostgreSQL
    ]

  -- Teardown
  teardownInsert connPostgreSQL
  teardownSelect connPostgreSQL

  -- Disconnect
  disconnect connPostgreSQL
  disconnect connODBC

benchBackend :: IConnection conn => String -> conn -> Benchmark
benchBackend backend conn = bgroup backend
  [ benchInsert conn 1000
  , benchSelectInt conn 10000
  -- , benchSelectInt32 conn 10000
  , benchSelectDouble conn 10000
  , benchSelectString conn 10000
  , benchSelectDate conn 10000
  , benchSelectTime conn 10000
  ]

--------------------
setupInsert :: IConnection conn => conn -> IO ()
setupInsert conn = do 
  run conn
    "CREATE TABLE testInsert (v1 INTEGER, v2 FLOAT, v3 CHAR(4))" []
  commit conn

benchInsert :: IConnection conn => conn -> Int -> Benchmark
benchInsert conn n = bench "Insert" $ nfIO $ do
  forM [1 .. n] $ \x ->
    run conn "INSERT INTO testInsert (v1, v2, v3) VALUES (?, ?, ?)"
      [ SqlInt32 (fromIntegral x)
      , SqlDouble (fromIntegral x)
      , SqlString (show x)
      ]
  commit conn
  run conn "DELETE FROM testInsert" []
  commit conn

teardownInsert :: IConnection conn => conn -> IO ()
teardownInsert conn = do
  run conn
    "DROP TABLE testInsert" []
  commit conn

--------------------
setupSelect :: IConnection conn => conn -> Int -> IO ()
setupSelect conn n = do
  run conn
    "CREATE TABLE testSelect (v1 INTEGER, v2 FLOAT, v3 CHAR(4), v4 DATE, v5 TIMESTAMP)" []
  replicateM n $
    run conn "INSERT INTO testSelect (v1, v2, v3, v4, v5) VALUES (?, ?, ?, ?, ?)"
      [ SqlInt32 1
      , SqlDouble 1.0
      , SqlString "test"
      , SqlLocalDate testDay
      , SqlLocalTime testTime
      ]
  commit conn

testDay :: Day
testDay = fromGregorian 2011 11 20

testTime :: LocalTime
testTime = LocalTime testDay (TimeOfDay 3 14 15.926536) -- 5897932)

benchSelect :: IConnection conn => conn -> Int -> Benchmark
benchSelect conn n = bench "Select" $ nfIO $ do
  quickQuery' conn "SELECT * FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn

benchSelectInt :: IConnection conn => conn -> Int -> Benchmark
benchSelectInt conn n = bench "SelectInt" $ nfIO $ do
  vss <- quickQuery' conn "SELECT v1 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Int)) vss == n)
    then return ()
    else error "benchSelectInt: Unexpected sum!"

benchSelectDouble :: IConnection conn => conn -> Int -> Benchmark
benchSelectDouble conn n = bench "SelectDouble" $ nfIO $ do
  vss <- quickQuery' conn "SELECT v2 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if ((sum . map (\[v] -> fromSql v :: Double)) vss == fromIntegral n)
    then return ()
    else error "benchSelectDouble: Unexpected sum!"

benchSelectString :: IConnection conn => conn -> Int -> Benchmark
benchSelectString conn n = bench "SelectString" $ nfIO $ do
  vss <- quickQuery' conn "SELECT v3 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if all (\[v] -> (fromSql v :: String) == "test") vss
    then return ()
    else error "benchSelectString: Unexpected String!"

benchSelectDate :: IConnection conn => conn -> Int -> Benchmark
benchSelectDate conn n = bench "SelectDate" $ nfIO $ do
  vss <- quickQuery' conn "SELECT v4 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if all (\[v] -> (fromSql v :: Day) == testDay) vss
    then return ()
    else error "benchSelectDate: Unexpected Date!"

benchSelectTime :: IConnection conn => conn -> Int -> Benchmark
benchSelectTime conn n = bench "SelectTime" $ nfIO $ do
  vss <- quickQuery' conn "SELECT v5 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  if all (\[v] -> (fromSql v :: LocalTime) == testTime) vss
    then return ()
    else error "benchSelectTime: Unexpected Time!"

teardownSelect :: IConnection conn => conn -> IO ()
teardownSelect conn = do
  run conn
    "DROP TABLE testSelect" []
  commit conn
