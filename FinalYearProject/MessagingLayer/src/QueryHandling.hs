module QueryHandling
    ( 
      dataSelect,
      tableSearch,
      symbolToID,
      parseURL,
      getRequestString,
      convertURI,
      getURI,
      evaluateRequest,
      validInterval,
      validSymbol,
      validStartTime,
      getRequestSymbol,
      getRequestInterval,
      getRequestStartTime,
      getRequestEndTime,
      getRequestLimit,
      extractTradingDataQuery,
      extractCandleStickQuery,
      GetRequest,
      extractTradingPairsQuery
           
    ) where

import qualified GetDatabaseValues
import qualified DatabaseExtraction as DE

-- External
import qualified Network.HTTP.Server as Server --for request objects
import qualified Network.HTTP.Base as Base -- for deconstructing request objects
import qualified Network.URI as URI -- for parsing http uri
import qualified Data.List.Split as DL-- for constructing the get request object
import qualified Data.List
import qualified Data.Either as Either -- for error handling



-- Data type for get get requests

data GetRequest = GetRequest {symbol :: String, interval :: String, startTime :: String, endTime :: String, limit :: Int} deriving (Show)


evaluateRequest :: String -> IO (String)
evaluateRequest request 
	| Data.List.length (DL.splitOneOf "= &" request) == 10 = verifyParams request
	| Data.List.length (DL.splitOneOf "= &" request) == 11 = return "Possible Internal Request"
	| Data.List.length (DL.splitOneOf "= &" request) == 1 = return "Possible Internal Command"
	| Data.List.length (DL.splitOneOf "= &" request) < 10 = return "Missing Param"
	| otherwise = return "Bad Length"


-- verify parameters of request
verifyParams :: String -> IO (String)
verifyParams request = do 
			let parsedRequest = parseURL request
			-- verify the trading symbol
			symbol <- DE.searchTradingPair $ getRequestSymbol parsedRequest
			--print("SYMBOL")
			--print(symbol)
			case symbol of
			   -- verify the interval
			   True -> do 
				    interval <- DE.searchTradingPair $ getRequestSymbol parsedRequest
				    case interval of
				       True -> return "Good Request"
				       False -> return "Bad Interval"
			   False -> return "Bad Symbol"
			

--- verify the requested interval
validSymbol :: String -> Bool
validSymbol request = True 


--- verify the requested interval
validInterval :: String -> Bool
validInterval request = True 

--- verify the requested Start Time
validStartTime :: String -> Bool
validStartTime request = True 

--- verify the requested interval
--validInterval :: String -> IO Bool
--validInterval request = True 


--- Helper functions for extracting field from the GetRequest Object in Http handling module
getRequestSymbol :: GetRequest -> String
getRequestSymbol request = symbol request

getRequestInterval :: GetRequest -> String
getRequestInterval request = interval request 

getRequestStartTime :: GetRequest -> String
getRequestStartTime request = startTime request 

getRequestEndTime :: GetRequest -> String
getRequestEndTime request = endTime request 

getRequestLimit :: GetRequest -> String
getRequestLimit request = show(limit request) 



--- Get URI From request Object

getURI :: Server.Request String -> URI.URI --(URI Data type from URI Module)
getURI request = Base.rqURI request


--- Convert a URI to string

convertURI :: URI.URI -> String
convertURI request = URI.uriQuery request

--- Get a http request as a string

getRequestString :: Server.Request String -> String
getRequestString requestObject = convertURI (getURI requestObject) 
 
 
--- Parse The URL into a get request

parseURL :: String -> GetRequest
parseURL url = GetRequest {symbol = (splitUrl!!1) , interval = splitUrl!!3 , startTime = splitUrl!!5 , endTime = splitUrl!!7 , limit = (read (splitUrl!!9) :: Int) } where
                splitUrl = DL.splitOneOf "= &" url -- Split the uri string at "=" and "&". Gives required elements at odd indicies {1,3,5,7,9}.




         
-- convert trading pair string to database Primary Key
symbolToID :: GetRequest -> String
symbolToID request = show(GetDatabaseValues.extractTradingPairIDString(symbol request))


--- 


-- Combine a get requests symbol and interval fields so we can query individual tables
tableSearch :: GetRequest -> String
tableSearch request = show(symbol request ++ interval request)


--- This is only a temp var
dataSelect = " _StartTime, _Open, _High, _Low, _Close, _Volume, _CloseTime, _QuoteVol, _NumTrades, _TakeBuyVol, _TakeQuoteVol "

candleSelect =  " _StartTime, _Low, _Open, _Close, _High, _Volume " -- Formatted for google chart api


tradingPairSelect = "*"

--- Query To extract trading data

extractTradingDataQuery :: GetRequest -> String
extractTradingDataQuery request = query where
                                            tradingPair = getRequestSymbol request -- extract tradingpair
                                            interval = getRequestInterval request -- extract the interval
                                            startTime = getRequestStartTime request -- extract startTime
                                            endTime = getRequestEndTime request -- extract end Time
                                            limit = getRequestLimit request -- extract the limit
                                            query = "SELECT" ++ dataSelect ++ "FROM " ++ tradingPair ++ interval ++ " LIMIT 1000"-- ++ --" WHERE _StartTime BETWEEN " ++ startTime ++ " AND " ++ endTime ++ " LIMIT " ++ limit
					    -- NB SQL Injection vulnerabilities arise from concatenated queries like this 


 

extractCandleStickQuery :: GetRequest -> String
extractCandleStickQuery request = query where
                                            tradingPair = getRequestSymbol request -- extract tradingpair
                                            interval = getRequestInterval request -- extract the interval
                                            startTime = getRequestStartTime request -- extract startTime
                                            endTime = getRequestEndTime request -- extract end Time
                                            limit = getRequestLimit request -- extract the limit
                                            query = "SELECT" ++ candleSelect ++ "FROM " ++ tradingPair ++ interval ++ " ORDER BY _StartTime DESC LIMIT " ++ limit  -- ++ --" WHERE _StartTime BETWEEN " ++ startTime ++ " AND " ++ endTime ++ " LIMIT " ++ limit


extractTradingPairsQuery :: GetRequest -> String
extractTradingPairsQuery request = "SELECT * FROM TradingPairs"
                                              
--ORDER BY _CloseTime DESC LIMIT 1





















