module HttpHandling
    ( 
      getResponse,
      errorResponse,
      sendResponse,
      handleRequest
    ) where

import qualified Connections -- keeps port open 
import qualified DatabaseExtraction as DatabaseExtraction -- Needed temp
import qualified QueryHandling as QH
import qualified DatabaseCreation
import qualified PopulationControlStructure as PCS
import qualified DatabasePopulation

-- External libs
import qualified Network.Socket as Socket -- needed for socket
import qualified Network.HTTP.Listen as Listen -- needed for Streams
import qualified Network.HTTP.Base as Base -- for HTTP response objects
import qualified Network.HTTP.Server as Server --for request objects
import qualified Data.List as DL

-- Data type to hold error code and message for bad http request

data Request = Request {body :: String, code :: (Int,Int,Int) } deriving (Show)



--- If the url contain a system command

systemCommands :: String -> Bool
systemCommands request = case request of 
                            "?CreateDatabase" -> True
                            "?Terminate" -> True
                            "?Populate" -> True
			    "?Intervals" -> True
			    "?TradingPairs" -> True	
                            otherwise -> False

--- Handle System commands
systemCommandHandling :: String -> Listen.Stream String -> Listen.Connection  -> Socket.Socket -> IO ()
systemCommandHandling command stream conn socket
	| command == "?CreateDatabase" = do
					  DatabaseCreation.createDatabase
					  sendResponseA (Request {body = "Database Has been created and populated.", code = (2,0,0)}) stream conn socket
	| command == "?Populate" = do
				    PCS.populate
				    sendResponseA (Request {body = "Database Has been populated.", code = (2,0,0)}) stream conn socket
	| command == "?Intervals" = getIntervalsResponse stream conn socket
	| command == "?TradingPairs" = getTradingPairsResponse stream conn socket
	| command == "?Terminate" = print("Goodbye!")


--- Send Http response 
sendResponse :: String -> (Int,Int,Int) -> Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
sendResponse responseBody responseCode  stream conn socket = do 
                                             let response = Base.Response {Base.rspCode = responseCode , Base.rspReason = "Why Not", Base.rspHeaders = [], Base.rspBody = responseBody }
                                             Listen.sendResponse stream response --- send HTTP Response 
					     Connections.closeStream stream
					     Connections.closeConnection conn 
                                             --Connections.purge stream conn socket --- Close all connections  

sendResponseA :: Request -> Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
sendResponseA request stream conn socket = do 
                                             let response = Base.Response {Base.rspCode = (code request) , Base.rspReason = "Why Not", Base.rspHeaders = [], Base.rspBody = (body request) }
                                             Listen.sendResponse stream response --- send HTTP Response 
					     Connections.closeStream stream
					     Connections.closeConnection conn 
                                             --Connections.purge stream conn socket --- Close all connections  


--- Create Http response body, get data from DB
getResponse :: String -> Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
getResponse sqlQuery stream conn socket = do
                                           --- update database table
                                           results <- DatabaseExtraction.extractCandle sqlQuery -- Extract From database   
                                           --- Create Response Body
                                           let responseBody = show(results)
                                           sendResponse responseBody (2,0,0) stream conn socket -- Send the response


getTradingPairsResponse :: Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
getTradingPairsResponse stream conn socket = do
                                              results <- DatabaseExtraction.extractSymbols --sqlQuery -- Extract From database   
                                              --- Create Response Body
                                              let responseBody = show(results)
                                              sendResponse responseBody (2,0,0) stream conn socket -- Send the response

getIntervalsResponse :: Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
getIntervalsResponse stream conn socket = do
                                            results <- DatabaseExtraction.extractIntervalsReq --sqlQuery -- Extract From database   
                                            --- Create Response Body
                                            let responseBody = show(results)
                                            sendResponse responseBody (2,0,0) stream conn socket -- Send the response



--- Validate a client request
handleRequest :: Server.Request String -> Listen.Stream String -> Listen.Connection  -> Socket.Socket  -> IO ()
handleRequest serverRequestObject stream conn socket = do
						  	let request = QH.getRequestString serverRequestObject -- Extract the URI from the request object
							    requestObject = QH.parseURL request 
                                       		  	-- Check the number of elements in the request
						  	requestType <- QH.evaluateRequest request
						  	serveRequest requestType request requestObject stream conn socket
						
--- Serve a client request
serveRequest :: String -> String -> QH.GetRequest -> Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
serveRequest requestType requestString requestObject stream conn socket  
	| requestType == "Bad Length" = print $ "Bad Length"
	| requestType == "Possible Internal Command" = case systemCommands requestString of
						           True -> systemCommandHandling requestString stream conn socket
						           False -> sendResponseA (Request {body = "You're Missing Some Parameters", code = (4,0,4) }) stream conn socket

	| requestType == "Possible Internal Request" = case (take 9 $ drop ((DL.length $ requestString) -9) requestString) == "&Internal" of
							  -- Send response, dont update the database
							  True -> getResponse(QH.extractCandleStickQuery requestObject) stream conn socket
							  False -> sendResponseA (Request {body = "You've got too many parameters", code = (4,0,4) }) stream conn socket
	| requestType == "Good Request" = do	
				           -- Update database table
					   DatabasePopulation.updateTable (QH.getRequestSymbol requestObject) (QH.getRequestInterval requestObject)
					   -- send Response 
					   getResponse(QH.extractCandleStickQuery requestObject) stream conn socket

	| requestType == "Bad Interval" = sendResponseA (Request {body = "We don't have data for that Interval", code = (4,0,4) }) stream conn socket
	| requestType == "Bad Symbol" = sendResponseA (Request {body = "We don't have data for that symbol", code = (4,0,4) }) stream conn socket
	| requestType == "Missing Param" = sendResponseA (Request {body = "You're Missing Some Parameters", code = (4,0,4) }) stream conn socket



                                                      
--- Create and send an response, in the case of an incorrectly formed http request of internal errors - error reports will be added later

errorResponse :: String -> (Int,Int,Int) -> Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
errorResponse errorMessage errorCode stream conn socket = do
                                       --- Create Response Obect
                                       let response = Base.Response {Base.rspCode = errorCode , Base.rspReason = "We think, therefore we're not sure", Base.rspHeaders = [], Base.rspBody = errorMessage }
                                       print (Base.rspBody response) --- SHow responce body (testing)
                                       Listen.sendResponse stream response --- send HTTP Response 
                                       Connections.purge stream conn socket --- Close all connections  


