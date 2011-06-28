{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.PostgreSQL

import Control.Monad (replicateM, forM, liftM)

main :: IO ()
main = do
  -- Connect
  connPostgreSQL <- connectPostgreSQL "host=localhost dbname=hdbc-test user=hdbc password=qwerfdsa"
  connODBC       <- connectODBC "DSN=HDBC"

  -- Setup
  setupInsert connODBC
  setupSelect connODBC 10000

  -- Benchmark
  defaultMain
    [ benchBackend "odbc"       connPostgreSQL
    , benchBackend "postgresql" connODBC
    ]

  -- Teardown
  teardownInsert connODBC
  teardownSelect connODBC

  -- Disconnect
  disconnect connPostgreSQL
  disconnect connODBC

benchBackend :: IConnection conn => String -> conn -> Benchmark
benchBackend backend conn = bgroup backend
  [ benchInsert conn 1000
  , benchSelectInt conn 10000
  , benchSelectDouble conn 10000
  , benchSelectString conn 10000
  ]

--------------------
setupInsert :: IConnection conn => conn -> IO ()
setupInsert conn = do 
  run conn
    "CREATE TABLE testInsert (v1 INTEGER, v2 FLOAT, v3 CHAR(64))" []
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
    "CREATE TABLE testSelect (v1 INTEGER, v2 FLOAT, v3 CHAR(64))" []
  forM [1 .. n] $ \x ->
    run conn "INSERT INTO testSelect (v1, v2, v3) VALUES (?, ?, ?)"
      [ SqlInt32 (fromIntegral x)
      , SqlDouble (fromIntegral x)
      , SqlString (show x)
      ]
  commit conn

benchSelect :: IConnection conn => conn -> Int -> Benchmark
benchSelect conn n = bench "Select" $ nfIO $ do
  quickQuery conn "SELECT * FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn

benchSelectInt :: IConnection conn => conn -> Int -> Benchmark
benchSelectInt conn n = bench "SelectInt" $ nfIO $ do
  vss <- quickQuery conn "SELECT v1 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  return $ map (\[v] -> fromSql v :: Int) vss


benchSelectDouble :: IConnection conn => conn -> Int -> Benchmark
benchSelectDouble conn n = bench "SelectDouble" $ nfIO $ do
  vss <- quickQuery conn "SELECT v2 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  return $ map (\[v] -> fromSql v :: Double) vss

benchSelectString :: IConnection conn => conn -> Int -> Benchmark
benchSelectString conn n = bench "SelectString" $ nfIO $ do
  vss <- quickQuery conn "SELECT v3 FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  return $ map (\[v] -> fromSql v :: String) vss

teardownSelect :: IConnection conn => conn -> IO ()
teardownSelect conn = do
  run conn
    "DROP TABLE testSelect" []
  commit conn
