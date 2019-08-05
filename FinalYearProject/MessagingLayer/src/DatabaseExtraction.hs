{-# LANGUAGE BangPatterns #-}
module DatabaseExtraction
    (
      extractTradingData,
      extractTradingPairs,
      searchTradingPair,
      searchInterval,
      getPairName,
      getIntervalName,
      extractIntervals,
      TradingPair,
      Interval,
      extractCandle,
      extractSymbols,
      extractIntervalsReq,
      extractLastCloseTime,
      CloseTime,
      deleteLastRow 
       
    ) where
import qualified DatabaseConnection as DBC

-- External
import qualified Database.MySQL.Simple as SQL-- SQL Interface
import qualified Database.MySQL.Simple.QueryResults as QueryResults -- for converting SQL values to haskell values
import qualified Database.MySQL.Simple.Result as SQLR -- for converting sql to haskell
import qualified Data.String as DS -- convert strings to SQL query
import qualified Data.List

import Data.Strings as S

--- SQL Query to search the trading pairs table for a given symbol
tradingPairSearchQuery = "select _ID,_TradingPair from TradingPairs where _TradingPair = "

--- SQL Query to search the Intervals table for a given interval
intervalSearchQuery = "select _ID,_Interval from Intervals where _Interval = "




--- sql QUERY TO EXTRACT HISTORICAL DATA
--sqlExtractCandlestick :: 
--sqlExtractCandlestick = "SELECT * FROM " ++ tradingPair ++ "WHERE _StartTime BETWEEN" ++ sTART "AND" ++ 

-- data type for database entries of trading pairs / symbols

data TradingPair = TradingPair {pairID :: Int, pairName :: String} deriving (Show)

-- data type for interval entries

data Interval = Interval {intervalId :: Int, intervalName :: String } deriving (Show)

-- data type for just the interva name
data IntervalName = IntervalName {name :: String} deriving (Show)

data CloseTime = CloseTime {time :: Int} deriving (Show)


-- Instance of Query results (Database.Mysql import) as type Trading pair. performs conversion from SQL values to haskell values

instance QueryResults.QueryResults TradingPair where
      convertResults [fa, fb] [va, vb] = TradingPair {pairID = a, pairName = b}
           where 
                !a = SQLR.convert fa va
                !b = SQLR.convert fb vb   
      convertResults fs vs = QueryResults.convertError fs vs 2

-- Instance of interval name; used to convert sql values to haskell vals

instance QueryResults.QueryResults IntervalName where
      convertResults [fa] [va] = IntervalName { name = a}
           where 
                !a = SQLR.convert fa va   
      convertResults fs vs = QueryResults.convertError fs vs 1


instance QueryResults.QueryResults CloseTime where
      convertResults [fa] [va] = CloseTime { time = a}
           where 
                !a = SQLR.convert fa va   
      convertResults fs vs = QueryResults.convertError fs vs 1


-- Instance of Query results (Database.Mysql import) as type Interval. performs conversion from SQL values to haskell values

instance QueryResults.QueryResults Interval where
      convertResults [fa, fb] [va, vb] = Interval {intervalId = a , intervalName = b}
           where 
                !a = SQLR.convert fa va
                !b = SQLR.convert fb vb 
      convertResults fs vs = QueryResults.convertError fs vs 2


--- Getter for TradingPair Object name
getPairName :: TradingPair -> String
getPairName pair = pairName pair


--- Getter for Interval Object name
getIntervalName :: Interval -> String
getIntervalName interval = intervalName interval


--- Search for a a given trading pair.
searchTradingPair :: String -> IO Bool
searchTradingPair symbol = do 
			    print("IN SEARCH TRADING PAIR")
                            print(symbol)
                            conn <- DBC.databaseConnection
                            resultsSQL <- SQL.query_ conn (DS.fromString (tradingPairSearchQuery ++ show(symbol)))
                            let results = resultsSQL :: [TradingPair] 
                            print(results)
                            print(Data.List.length results)  
                            return ((Data.List.length results) == 1)


--- Search for a a given Interval
searchInterval :: String -> IO Bool
searchInterval interval = do 
                            conn <- DBC.databaseConnection
                            resultsSQL <- SQL.query_ conn (DS.fromString (intervalSearchQuery ++ show(interval)))
                            print(interval)
                            let results = resultsSQL :: [Interval]
                            print(results)
                            print(Data.List.length results)
                            return ((Data.List.length results) == 1)


-- Take all trading Pairs from the DB

extractTradingPairs :: IO [TradingPair]
extractTradingPairs = do 
                        conn <- DBC.databaseConnection
                        resultsSQL <- SQL.query_ conn (DS.fromString "select _ID,_TradingPair from TradingPairs")
                        let results = resultsSQL :: [TradingPair]		
                        return results


extractSymbols :: IO [(Int,String)]
extractSymbols = do 
                   conn <- DBC.databaseConnection
                   resultsSQL <- SQL.query_ conn (DS.fromString "select _ID,_TradingPair from TradingPairs")
                   let results = resultsSQL :: [(Int,String)]		
                   return results



extractIntervalsReq :: IO [(Int,String)]
extractIntervalsReq = do 
                       conn <- DBC.databaseConnection
                       resultsSQL <- SQL.query_ conn (DS.fromString "select _ID,_Interval from Intervals")
                       let results = resultsSQL :: [(Int,String)]		
                       return results





-- Take all trading Pairs from the DB

extractIntervals :: IO [Interval]
extractIntervals = do 
                    conn <- DBC.databaseConnection
                    resultsSQL <- SQL.query_ conn (DS.fromString "select _ID,_Interval from Intervals")
                    let results = resultsSQL :: [Interval]		
                    return results





-- Extract rows of data (excluding CryptoID, ExchangeID , Interval ) from the (test) table Historical Data


extractTradingData :: String -> IO [(Int,Float,Float,Float,Float,Float,Int,Float,Int,Float,Float)]
extractTradingData sqlQuery = do
                             conn <- DBC.databaseConnection 
                             resultsSQL <- SQL.query_ conn (DS.fromString sqlQuery) --"select * from Hourly_Data"
                             let results = resultsSQL :: [(Int,Float,Float,Float,Float,Float,Int,Float,Int,Float,Float)] 
                             return results


deleteLastRow :: String -> IO ()
deleteLastRow table = do
                       conn <- DBC.databaseConnection 
                       resultsSQL <- SQL.execute_ conn (DS.fromString ("DELETE FROM " ++ table ++ " ORDER BY _CloseTime DESC LIMIT 1"))
                       print("Last Entry Removed")


extractCandle :: String -> IO (String)--[Int,Float,Float,Float,Float]---[(Int,Float,Float,Float,Float)]
extractCandle sqlQuery = do
                             conn <- DBC.databaseConnection 
                             resultsSQL <- SQL.query_ conn (DS.fromString sqlQuery) --"select * from Hourly_Data"
                             let results = resultsSQL :: [(Int,Float,Float,Float,Float,Float)] 
                                 x = (show results) 
                                 y = S.strReplace "(" "[" x
                                 z = S.strReplace ")" "]" y 
                             --print(z)
                             --let m = read z :: [Int,Float,Float,Float,Float]
                             --print(m)
                             return z


extractLastCloseTime :: String -> IO (Maybe Int)
extractLastCloseTime table = do
                              print(table)
			      conn <- DBC.databaseConnection 
                              resultsSQL <- SQL.query_ conn (DS.fromString ("SELECT _CloseTime FROM " ++ table ++ " ORDER BY _CloseTime DESC LIMIT 1"))
                              let results = resultsSQL :: [CloseTime]
                             -- print(time (results!!0))   
                              --return results
                              case (Data.List.length results) of
                                 0 -> return Nothing
                                 otherwise -> return (Just(time (results!!0)) )
			   --   print("HIJGIGRWJGPRJJRGOJ")
                           --   return(time (results!!0))


--ORDER BY ID DESC LIMIT 1

















