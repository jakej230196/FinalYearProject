{-# LANGUAGE OverloadedStrings #-}
module Main where

import Connections -- maintain connection
import HttpHandling -- Constructing HTTP Response
import QueryHandling -- Constructing SQL Queries from URI
import DatabaseExtraction --- TEMP
import DatabasePopulation -- TEMP
import DatabaseCreation -- functions to create database tables dynamically


-- External Libraries
import Network.Socket -- for Sockets
import Network.HTTP.Listen -- For Streams
import Data.Typeable


--- Loop Server Logic

server :: Socket -> IO ()
server socket = do 
                  conn <- Connections.receiveConn socket -- Wait for client connections on the given port (8000)
                  stream <- Connections.dataStream conn -- Datastream connection
                  request <- Connections.readRequest stream -- get client request

                  --- Check if a valid request was made
                  case request of
                        
                     Right a -> do 
                                 --- Create and send response
                                 HttpHandling.handleRequest a stream conn socket
                                 server socket
                      
                     Left e -> do HttpHandling.sendResponse "There is something wrong with your query, we're not quite sure what " (4,0,0) stream conn socket
                                  server socket -- repeat 

main :: IO ()
main = do   
         socket <- prepSocket 8000 -- Initialize Socket
         server socket -- Loop server Logic

         
         
         









{-
main :: IO ()
main = do 
         socket <- socket AF_INET Stream 0  --- create a socket
         setSocketOption socket ReuseAddr 1 --- Make the socket re-useable
         bind socket (SockAddrInet 8000 iNADDR_ANY) -- listen on port 8000
         listen socket 5 --- max of 5 connections
         listenLoop socket --- loop through server logic

listenLoop :: Socket -> IO ()
listenLoop socket = do 
                      sockConn <- acceptConnection socket -- accept a connection --- TAKES A SOCKET AND RETURNS A CONNECTION
                      handleS sockConn
                      listenLoop socket --- Repeat 
                      --close socket  

handleS :: (Socket, SockAddr) -> Socket -> IO ()
handleS (socket, _) = do 
                    request <- receiveRequest socket --- listen for request
                    print(request) -- print the request 
                    close socket   
                     

                         -- serveConn conn -- serve the connection
                         --listenLoop socket --- Repeat
{-
serveConn :: (Socket, SockAddr) -> IO ()
serveConn (socket, _) = do 
                          stream <- openStream (socket)
                          send socket "HI"
                          close socket        
-}


-}












--data SocketAddress = SockAddrInet 80 127.0.0.1:8000


---type handlerA = print("vokookok")

--main :: IO ()
--main = server handlerA

{-
--- Local Host:  127.0.0.1:8000

data TradingPair = BTCUSDT | ETHUSDT

tradingPairRequest :: TradingPair -> String
tradingPairRequest BTCUSDT = "Hello, You Requested Data For BTCUSDT"
tradingPairRequest ETHUSDT = "Hello, You Requested Data For ETHUSDT"


instance FromReqURI TradingPair where
        fromReqURI pair =
                 case map toLower pair of
                 "ethusdt" -> Just ETHUSDT
                 "btcusdt" -> Just BTCUSDT
                 _ -> Nothing 


searchURI :: ServerPart String
searchURI = 
	  do symbol <- look "SYMBOL"
             interval <- look "INTERVAL"
             startTime <- look "STARTTIME"
             endTime <- look "ENDTIME"
             entries <- look "LIMIT" 
              
             return (constructRequest "klines" symbol interval startTime endTime entries)

postHandler :: ServerPart Response 
postHandler = require (populateDB req symbol interval startTime endTime limit) $ \x ->
                do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
                   msum [
                          dir "ETHUSDT" $ ok $ toResponse x
                    
                         , ok $ toResponse "Nothing" 
   
                        ] 
                where 
                     req = "klines"
                     symbol = "ETHUSDT"
                     interval = "1h"
                     startTime = "1539032470000"
                     endTime = "1749032470000"
                     limit = "2"   


data GetRequest = GetRequest {req :: String, symbol :: String, interval :: String, startTime :: String, endTime :: String, limit :: String }


requestParams :: ServerPart Response
requestParams = do
                  --
	          let req = "klines"
                  symbol <- look "SYMBOL"
                  interval <- look  "INTERVAL"
                  startTime <- look "STARTTIME"
                  endTime <- look "ENDTIME"
                  limit <- look "LIMIT"
                  return (GetRequest req symbol interval startTime endTime limit)
                  



bodyPolicy :: BodyPolicy
bodyPolicy = (defaultBodyPolicy "/tmp/" 0 1000 1000)




getHandler :: ServerPart Response 
getHandler = do 
               x <- lift (databaseExtract symbol interval startTime endTime limit) 
               decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000) 
	       msum[ (ok $ toResponse $ (show x) )]
               where 
                    req = "klines"
                    symbol = "ETHUSDT"
                    interval = "1h"
                    startTime = "1539032470000"
                    endTime = "1749032470000"
                    limit = "2"



getHandlerTest :: ServerPart Response
getHandlerTest = do 
                   x <- decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
                   ok $ toResponse $ show x

main :: IO ()
main = simpleHTTP nullConf $ msum 
       [ mzero

        ,do method GET
            decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000) 
            getHandler

        ,do method POST
            postHandler 
       ]

{-
main :: IO ()
main = simpleHTTP nullConf insertHandler

-}
{-

main = simpleHTTP nullConf $ msum 
       [ do method GET
	    --path $ \tradingPair -> ok $ (tradingPairRequest tradingPair)
            searchURI
	    --handlers 
         , do method POST 
              ok $ "thank you"          
        ]				  

-}





















{- THIS RECOGNISES A TRADING PAIR IN THE URI 

import Happstack.Server (FromReqURI(..),nullConf, simpleHTTP, ok, dir, path)
import Control.Monad (msum)
import Data.Char (toLower)

--class FromReqURI a where fromReqURI :: String -> Maybe a



data TradingPair = BTCUSDT | ETHUSDT

tradingPairRequest :: TradingPair -> String
tradingPairRequest BTCUSDT = "Hello, You Requested Data For BTCUSDT"
tradingPairRequest ETHUSDT = "Hello, You Requested Data For ETHUSDT"


instance FromReqURI TradingPair where
        fromReqURI pair =
                 case map toLower pair of
                 "ethusdt" -> Just ETHUSDT
                 "btcusdt" -> Just BTCUSDT
                 _ -> Nothing 


main :: IO ()
main = simpleHTTP nullConf $ path $ \tradingPair -> ok $ (tradingPairRequest tradingPair)

-}



{-
handlers :: ServerPart Response
handlers = require (populateDB "klines" "ETHUSDT" "1h" "1539032470000" "1749032470000" "2") $ \x ->
           do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
              msum [
                     dir "ETHUSDT" $ ok $ toResponse x
                    
                    , ok $ toResponse "Nothing" 
   
                   ] 


-}
{-

insertHandler :: String -> String -> String -> String -> String -> String -> ServerPart Response
insertHandler = require (populateDB "klines" "ETHUSDT" "1h" "1539032470000" "1749032470000" "2") $ \x ->
                do decodeBody (defaultBodyPolicy "/tmp/" 0 1000 1000)
                   msum [
                          dir "ETHUSDT" $ ok $ toResponse x
                    
                         , ok $ toResponse "Nothing" 
   
                        ]       

-}



{-
--- Get request parameters --- 

searchURI :: ServerPart String
searchURI = 
	  do symbol <- look "SYMBOL"
             interval <- look "INTERVAL"
             startTime <- look "STARTTIME"
             endTime <- look "ENDTIME"
             entries <- look "LIMIT" 
              
             return (constructRequest "klines" symbol interval startTime endTime entries)

-}
-}

{-
--- Read Request From Data stream
readRequest :: Stream String -> URI.URI
readRequest stream = do 
                       request <- receiveRequest stream
                       case request of 
                         Left err -> 
                         Right msg -> getURI request
-}                      




--- temp
{-
intervals = ["1m","3m","5m","15m","30m","1h","2h","4h","6h","8h","12h","1d","3d","1w","1M"]

--tradingPairsLoop = [ tradingPairsIntervalLoop tradingPair | tradingPair <- tradingPairs]

tradingPairsIntervalLoop tradingPair = [tradingPairsConcatLoop tradingPair interval [] | interval <- DatabasePopulation.intervals ]

tradingPairsConcatLoop tradingPair interval acc  = ( tradingPair ++ interval) -- :( tradingPairsConcatLoop tradingPair interval acc )


--- Get TradingPair List 
populateTradingPairs :: IO ()
populateTradingPairs = do
			-- Get exchange Info
			apiResponse <- httpBS "https://api.binance.com/api/v3/ticker/price"
		       -- print(Data.List.length (S8.unpack(getResponseBody apiResponse))) 
			let body =  (S8.unpack(getResponseBody apiResponse))
			let x = splitOn "symbol" body -- filter (isUpper)
			let tradingPairs = tail([ filter (isUpper) z | z <- x])
			print(tradingPairs)
			--let p = (["(" ++ n ++ ") | n <- tradingPairs])
		       --- print(p) 
			--DatabasePopulation.insertTradingPairs (show(tradingPairs)) 
			--print (show(tradingPairs))
		       -- let pairsIntervals = ([ x ++ show(y) | y <- testIntervals])
		       -- print(pairsIntervals) 
		       -- let l = [ tradingPairsIntervalLoop tradingPair | tradingPair <- tradingPairs]
		       -- print l

-}













