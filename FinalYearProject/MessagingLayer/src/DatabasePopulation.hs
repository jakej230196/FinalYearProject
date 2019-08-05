{-# LANGUAGE BangPatterns #-}
module DatabasePopulation where

import qualified CreateRequest -- imports the function constructRequest()
import qualified ParseApiResponse --
import qualified GetDatabaseValues --
import qualified DatabaseConnection as DBC
import qualified DatabaseExtraction

-- External
import qualified Network.HTTP.Simple as H --Imports functions for making API request and response objects
import qualified Database.MySQL.Simple as SQL-- SQL Interface
import qualified Data.String as DS -- convert strings to SQL query
import Data.List 
import Data.Typeable
import Data.List.Split
import qualified Data.ByteString.Char8 as B8 -- Needed To Conver ByteString To String 

--data CandleStick = CandleStick {startTime :: String, open :: Float, high :: Float, low :: Float, close :: Float, closeTime :: Int } deriving (Show)




getTime :: H.Response B8.ByteString -> Int 
getTime apiTime = read (init (DatabasePopulation.removeN 14 (B8.unpack(H.getResponseBody apiTime)))) :: Int

removeN :: Int -> [x] -> [x] 
removeN num lst | num <= 0 = lst
removeN _ [] = []
removeN num (_:lst) = removeN (num-1) lst 



updateTable :: String -> String -> IO ()
updateTable symbol interval = do 
                               timeResponse <- H.httpBS (H.parseRequest_ "https://api.binance.com/api/v1/time") --"https://api.binance.com/api/v1/time"
                               let serverTime = DatabasePopulation.getTime timeResponse
                               startTime <- DatabaseExtraction.extractLastCloseTime (symbol++interval)
                             --  print("UPDATE TABLE")
                             --  print(typeOf startTime)
                               print("This is server time")
                               print(serverTime)
                               print("This is startTime ")
                               print(startTime)
                               case startTime of
                                  Just a -> do
                                             DatabaseExtraction.deleteLastRow(symbol++interval)
                                             updateLoop symbol interval a serverTime --print("GGG")--updateLoop symbol interval startTime serverTime
                                  Nothing-> print("HHHH")
                             --  print(time)
                            --   updateLoop symbol interval startTime serverTime --print("GGG")--updateLoop symbol interval startTime serverTime   
                                

                    
updateLoop :: String -> String -> Int -> Int -> IO ()
updateLoop tradingPair interval startTime endTime 
   | startTime >= endTime = print(tradingPair ++ interval ++ "Time Loop Finished")
   | otherwise = do
                   print("TIME LOOP")
                   newStartTime <- populateDB "klines" tradingPair interval startTime endTime 1000 -- get end time of last entry of response
                   updateLoop tradingPair interval (read newStartTime :: Int) endTime -- repeat






insertStatement :: String -> String
insertStatement table = "insert into " ++ table ++ " (_StartTime,_Open,_High,_Low,_Close,_Volume,_CloseTime,_QuoteVol,_NumTrades,_TakeBuyVol,_TakeQuoteVol,_Exchange) values (?,?,?,?,?,?,?,?,?,?,?,?)" 
                        
tradingPairInsert = "insert into TradingPairs (_TradingPair) values (?)"

intervalInsert = "insert into Intervals (_Interval) values (?)"


-- Insertion into the (Testing) table Historical_Data

databaseInsert :: String -> [(Int,Float,Float,Float,Float,Float,Int,Float,Int,Float,Float,Int)] -> IO ()
databaseInsert table vals = do
                             conn <- DBC.databaseConnection --- Establish Database Connection
                             SQL.executeMany conn (DS.fromString (insertStatement table)) (vals) --- Execute Insert Statement
                             SQL.close conn --- Close Database Connection
                             print("Inserted")




--- Insert Trading pair entries into the trading pairs table
insertTradingPairs :: [String] -> IO ()
insertTradingPairs tradingPairs = do 
                                   conn <- DBC.databaseConnection --- connect
                                   SQL.execute conn (DS.fromString tradingPairInsert) (SQL.Only (head tradingPairs))
                                   SQL.close conn
                                   --print("Succesfull")
                                   case length (tail tradingPairs) of 
                                      0 -> print("Done")--SQL.close conn --print("Done")
                                      otherwise -> insertTradingPairs (tail tradingPairs)
--- Insert Trading pair entries into the trading pairs table
insertExchanges :: [String] -> IO ()
insertExchanges exchanges = do 
                           conn <- DBC.databaseConnection --- connect
                           SQL.execute conn (DS.fromString "insert into Exchanges (_Exchange) Values (?)") (SQL.Only (head exchanges)) --(exchange)
                           SQL.close conn
                           --print("Succesfull")
                           case length (tail exchanges) of 
                              0 -> print("Done")--SQL.close conn --print("Done")
                              otherwise -> insertExchanges (tail exchanges)

--- Insert Interval entries into the trading pairs table
insertIntervals :: [String] -> IO ()
insertIntervals intervals = do 
                             conn <- DBC.databaseConnection --- connect
                             SQL.execute conn (DS.fromString intervalInsert) (SQL.Only (head intervals))
                             SQL.close conn
                             --print("Succesfull")
                             case length (tail intervals) of 
                                0 -> print("Done")--SQL.close conn --print("Done")
                                otherwise -> insertIntervals (tail intervals)
 

-- Mismatch between Monthly interval


populateDB :: String -> String -> String -> Int -> Int -> Int -> IO (String)
populateDB req symbol interval startTime endTime limit = do 
                 
               --- Handle the mismatch between the monthly interval: "1H" on exchange and "1month" in the database due to duplicating "1m" (1min) interval
               case interval of
                 "1month" -> populateDB req symbol "1M" startTime endTime limit
                 otherwise -> do
	                       --- Construct API request
	                       let apiRequest = H.parseRequest_ (CreateRequest.constructRequest req symbol interval startTime endTime limit)
	                       --- Get API Response
	                       apiResponse <- H.httpBS apiRequest
			       --- check for empty response body
			       case B8.unpack(H.getResponseBody apiResponse) of
				  "[]" -> do 
					   print("Empty Body")
					   return (show endTime)
				  otherwise -> do 
						print("Good Body")	
	                       			--- Creates a list of tuples ready for DB insertion
                               			let parsedBody = read (ParseApiResponse.parseResponseString (H.getResponseBody apiResponse) "1") :: [(Int,Float,Float,Float,Float,Float,Int,Float,Int,Float,Float,Int)]

                               			-- Handle monthly interval mismatch
                               			case interval of
                                 		   "1M" -> do
                                          		    -- Insert response to DB   
                                          		    databaseInsert (symbol++"1month") parsedBody
                                          		    return(splitOn "," (show(last parsedBody))!!6) -- return the end time of the last entry 
                                 		   otherwise -> do 
	                                       			--- Insert Api response values into the database
                                               			 print(symbol++interval)  
                                               			 databaseInsert (symbol++interval) parsedBody
                                               			 return(splitOn "," (show(last parsedBody))!!6) -- return the end time of the last entry





