{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Criterion.Main (Benchmark, bench, defaultMain, nfIO, bgroup)

import Database.HDBC
import Database.HDBC.ODBC
import Database.HDBC.PostgreSQL

import System.Console.CmdArgs
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
  , benchSelect conn 10000
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
  quickQuery' conn "SELECT * FROM testSelect LIMIT ?" [SqlInt32 (fromIntegral n)]
  commit conn
  run conn "DELETE FROM testSelect" []
  commit conn

teardownSelect :: IConnection conn => conn -> IO ()
teardownSelect conn = do
  run conn
    "DROP TABLE testSelect" []
  commit conn

{-
Installing HDBC-PostgreSQL
--------------------------

The first step is to get PostgreSQL up and running. Next we'll need to set up a
database for the ODBC test. I'll be using PostgreSQL, which can be installed
using:

    sudo aptitude install postgresql postgresql-contrib postgresql-bin libpg-dev

More detailed instructions can be found on [Linode Library][1], and there's
plenty of information about how postgresql works in the [Slicehost Articles][2].

For the purposes of this preformance analysis, we'll create a user called
`hdbc`, and a database `hdbc-test`:

    CREATE ROLE hdbc WITH LOGIN ENCRYPTED PASSWORD 'password';
    CREATE DATABASE hdbc-test WITH OWNER hdbc ENCODING 'utf8';

Now we need to install the HDBC backend for postgresql:

    cabal install HDBC-postgresql

To test that HDBC is able to talk to postgresql, we'll just fire up a simple
ghci session:

    ghci> :m + Database.HDBC Database.HDBC.PostgreSQL
    ghci> conn <- connectPostgreSQL "host=localhost dbname=hdbc-test user=hdbc password=password"
    ghci> hdbcDriverName conn
    "postgresql"
    ghci> hdbcClientVer conn
    "8.4.8"

If that hasn't worked then I suggest you look over your configuration again.

Installing HDBC-ODBC
--------------------

Now that PostgreSQL is up and running, we can start adding a connection using
ODBC.
First make sure that you have unixodbc-dev installed on your linux machine:

    sudo aptitude install unixodbc unixodbc-dev unixodbc-bin

Getting ODBC to work with postgresql is simple when you know how: you must
make ODBC aware of your database drivers, and you need to supply some
information to allow ODBC to connect to the database.

Here's a diagram from the ODBCConfig tool that describes how the process works:

     Application
         |
    Driver Manager --- odbc.ini --- Config
         |                             |
       Driver                     odbcinst.ini
         |
    Database System

I can recommend looking at the [unixODBC documentation][3] to get things set up.
Use the `ODBCConfig` tool to get your ODBC connection set up, and use
the `DataManager` to verify that the connection works.

Next install HDBC-odbc from cabal:

    cabal install HDBC-odbc

Finally, we can fire up ghci to test the connection to our database via ODBC:

    ghci> :m + Database.HDBC Database.HDBC.ODBC
    ghci> conn <- connectODBC "DSN=HDBC"
    ghci> hdbcDriverName conn
    "odbc"
    ghci> hdbcClientVer conn
    "03.52"

With the basic onfiguration out of the way, we can bundle this up into an
application that connects to a database via ODBC.

[1]: http://library.linode.com/databases/postgresql
[2]: http://articles.slicehost.com/postgresql
[3]: http://www.unixodbc.org/odbcinst.html

-}
