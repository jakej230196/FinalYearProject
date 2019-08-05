-- {-# LANGUAGE OverloadedStrings #-}
module DatabaseConnection where

import Database.MySQL.Simple as SQL-- SQL Interface
import Database.MySQL.Base
import Database.MySQL.Base.Types
import Data.ByteString


-- Establsh a connection to the database and return the connection object



-- LOCAL XAMPP SQL SERVER http://localhost:3306/
{-
databaseConnection :: IO SQL.Connection
databaseConnection = do
                       conn <- SQL.connect SQL.defaultConnectInfo --- Connect to the database 
                         { SQL.connectHost =  "127.0.0.1" --- set Connection host
                         , SQL.connectUser = "root" --- Database User 
                         , SQL.connectPassword = ""
                         , SQL.connectDatabase = "HistoricalData" --- Database Name      
                         }
                       return conn --- Return connection object

-}


--- HOME NETWORK

{-
databaseConnection :: IO SQL.Connection
databaseConnection = do
                       conn <- SQL.connect SQL.defaultConnectInfo --- Connect to the database 
                         { SQL.connectHost =  "192.168.0.84" --- set Connection host
                         , SQL.connectUser = "root" --- Database User 
                         , SQL.connectPassword = ""
                         , SQL.connectDatabase = "HistoricalData" --- Database Name      
                         }
                       return conn --- Return connection object


-}

--  Local

databaseConnection :: IO SQL.Connection
databaseConnection = do
                       conn <- SQL.connect SQL.defaultConnectInfo --- Connect to the database 
                         { SQL.connectHost =  "192.168.0.37" --- set Connection host
                         , SQL.connectUser = "Jake" --- Database User 
                         , SQL.connectPort = 3306
                         , SQL.connectPassword = "K5m8Cr9JBlueBox1"
                         , SQL.connectDatabase = "tradingdata" --- Database Name      
                         }
                       return conn --- Return connection object





--  SUSSEX SERVER
{-
databaseConnection :: IO SQL.Connection
databaseConnection = do
                       conn <- SQL.connect SQL.defaultConnectInfo --- Connect to the database 
                         { SQL.connectHost =  "mysql.student.sussex.ac.uk" --- set Connection host
                         , SQL.connectUser = "jj256" --- Database User 
                         , SQL.connectPassword = "bluebox1"
                         , SQL.connectDatabase = "jj256" --- Database Name      
                         }
                       return conn --- Return connection object

-}

-- -}

{-

-- AZURE SERVER
 
  
databaseConnection :: IO SQL.Connection
databaseConnection = do
                       conn <- SQL.connect SQL.defaultConnectInfo --- Connect to the database 
                         { SQL.connectHost =  "fypjj256.database.windows.net" --- set Connection host
                         , SQL.connectPort =  1433
                         , SQL.connectUser = "jj256" --- Database User 
                         , SQL.connectPassword = "K5m8Cr9j"
                         , SQL.connectDatabase = "HistoricalData" --- Database Name  
                         , SQL.connectOptions = [(ConnectTimeout 6000), (ClientIP "139.184.223.139")]     
                         }
                       return conn --- Return connection object
-}
