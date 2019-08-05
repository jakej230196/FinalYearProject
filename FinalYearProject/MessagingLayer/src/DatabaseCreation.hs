{-# LANGUAGE OverloadedStrings #-}
module DatabaseCreation
    ( 
      createTradingPairsTable,
      createIntervalsTable,
      intervals,
      sqlCreateDataTable,
      sqlCreateTable,
      createDataTables,
      createDatabase
    ) where

import qualified DatabaseConnection as DBC
import qualified Database.MySQL.Simple as SQL-- SQL Interface
import qualified Data.String as DS -- convert strings to SQL query
import qualified DatabasePopulation
import qualified GetDataTables
import qualified GetTradingPairs



intervals = ["1m","3m","5m","15m","30m","1h","2h","4h","6h","8h","12h","1d","3d","1w","1month"]

-- SQL Query to create a trading pairs table
sqlCreateTableTradingPairs = "CREATE TABLE IF NOT EXISTS TradingPairs (_ID INT(11) AUTO_INCREMENT NOT NULL,_TradingPair VARCHAR(12) NOT NULL, PRIMARY KEY (_ID))"

-- SQL Query to create a intervals table
sqlCreateTableIntervals = "CREATE TABLE IF NOT EXISTS Intervals (_ID INT(11) AUTO_INCREMENT NOT NULL,_Interval VARCHAR(12) NOT NULL, PRIMARY KEY (_ID))"


-- SQL Query to create a intervals table
sqlCreateTableExchanges = "CREATE TABLE IF NOT EXISTS Exchanges (_ID INT(11) AUTO_INCREMENT NOT NULL,_Exchange VARCHAR(20) NOT NULL, PRIMARY KEY (_ID))"


-- Work around for (linux) sql server duplicate constrant name errors

sqlCreateDataTable :: String -> String
sqlCreateDataTable table = "CREATE TABLE IF NOT EXISTS " ++ table ++ " (_StartTime BIGINT(20) NOT NULL,_Open DECIMAL(65,12) NOT NULL,_High DECIMAL(65,12) NOT NULL,_Low DECIMAL(65,12) NOT NULL,_Close DECIMAL(65,12) NOT NULL,_Volume DECIMAL(65,12) NOT NULL,_CloseTime BIGINT(20) NOT NULL,_QuoteVol DECIMAL(65,12) NOT NULL,_NumTrades INT(11) NOT NULL,_TakeBuyVol DECIMAL (65,12) NOT NULL,_TakeQuoteVol DECIMAL (65,12) NOT NULL,_Exchange INT(11) NOT NULL, PRIMARY KEY (_StartTime,_Exchange,_CloseTime),CONSTRAINT FK_"++table++ "_Exchange FOREIGN KEY _Exchange (_Exchange) REFERENCES Exchanges (_ID) )"





createDataTables :: [[String]] -> IO ()
createDataTables tables = do 
                           sqlCreateDataTables (head tables)
                           case length (tail tables) of
                              0 -> print "Finished"
                              otherwise -> createDataTables (tail tables) 
                               
sqlCreateDataTables :: [String] -> IO ()
sqlCreateDataTables tables = do 
                              conn <- DBC.databaseConnection
                              SQL.execute_ conn (DS.fromString (sqlCreateDataTable (head(tables))))
                             -- SQL.close conn
                              case length (tail tables) of
                                 0 -> print "Done"
                                 otherwise -> sqlCreateDataTables (tail tables)  
                         
                                  
--- Construct the database from scratch
createDatabase :: IO ()
createDatabase = do
                  -- Create the Exchanges table 
                  DatabaseCreation.createExchangesTable
                  DatabasePopulation.insertExchanges ["Binance"]  
                  print("Exchanges Table Created") 
                  -- Create the trading pairs table 
                  DatabaseCreation.createTradingPairsTable
                  print("TradingPairs Table created")
                  -- Create the Intervals Table
                  DatabaseCreation.createIntervalsTable
                  print("Intervals Table Created")  
                  -- Populate the trading pairs table
                  tradingPairs <- GetTradingPairs.getTradingPairs -- get a list of trading pairs
                  DatabasePopulation.insertTradingPairs tradingPairs  -- Populate Trading Pairs 
                  print("TradingPairs Table Populated")
                  -- Populate the Intervals Table 
                  DatabasePopulation.insertIntervals DatabaseCreation.intervals
                  print("Intervals Table Populated")
                  --- Get a list of trading pairs and intervals from the database
                  tables <- GetDataTables.getDataTables
                  --- Create a table for each Trading pair and interval combination
                  DatabaseCreation.createDataTables tables
                  print("Data Tables Created")   


{-
sqlCreateDataTables :: [(String,Int)] -> IO ()
sqlCreateDataTables x = "jj" 
-}

sqlCreateTable :: String -> IO ()
sqlCreateTable createQuery = do
                             conn <- DBC.databaseConnection
                             SQL.execute_ conn (DS.fromString createQuery) 
                             SQL.close conn 

createTradingPairsTable :: IO ()
createTradingPairsTable  = do
                            conn <- DBC.databaseConnection --- Establish Database Connection
                            SQL.execute_ conn (DS.fromString sqlCreateTableTradingPairs) --- Execute Insert Statement
                            SQL.close conn --- Close Database Connection

createIntervalsTable :: IO ()
createIntervalsTable  = do
                          conn <- DBC.databaseConnection --- Establish Database Connection
                          SQL.execute_ conn (DS.fromString sqlCreateTableIntervals) --- Execute Insert Statement
                          SQL.close conn --- Close Database Connection

createExchangesTable :: IO ()
createExchangesTable  = do
                          conn <- DBC.databaseConnection --- Establish Database Connection
                          SQL.execute_ conn (DS.fromString sqlCreateTableExchanges) --- Execute Insert Statement
                          SQL.close conn --- Close Database Connection


{-
-- Insertion into the (Testing) table Historical_Data

databaseInsert :: [(Int,Float,Float,Float,Float,Float,Int,Float,Int,Float,Float,Int,Int,Int)] -> IO ()
databaseInsert vals = do
                        conn <- DBC.databaseConnection --- Establish Database Connection
                        SQL.executeMany conn (DS.fromString insertStatement) (vals) --- Execute Insert Statement
                        SQL.close conn --- Close Database Connection

-}

