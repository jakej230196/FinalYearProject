{-# LANGUAGE OverloadedStrings #-}
module GetTradingPairs
    ( 
      getTradingPairs,
      getRecentBinancePrice,
      extractResponseBody,
      splitSymbol,
      filterTradingPairs,
    ) where

import qualified Data.ByteString.Char8 as S8 -- Needed To Conver ByteString To String 
import qualified Network.HTTP.Simple as N.HTTP -- Needed For The Function 'HttpBS' & 'getResponseBody'
import qualified Data.Strings as Strings-- Needed for the 'strReplace' Function
import qualified Data.List.Split as List-- Needed For Slicing Strings
import qualified Data.Char as Chars-- needed for the "isUpper" function


-- SQL Query to create a trading pairs table
sqlCreateTableTradingPairs = "CREATE TABLE IF NOT EXISTS TradingPairs (_TradingPair VARCHAR(12) NOT NULL, PRIMARY KEY (_TradingPair))"

--- Get the most recent price of all trading pairs from the Binacne exchange

getRecentBinancePrice :: IO (N.HTTP.Response S8.ByteString)
getRecentBinancePrice =  N.HTTP.httpBS  "https://api.binance.com/api/v3/ticker/price"


--- Extract and unpack api response body

extractResponseBody :: (N.HTTP.Response S8.ByteString) -> String
extractResponseBody apiResponse =  S8.unpack $ N.HTTP.getResponseBody apiResponse


--- Split the response Body on occurenses of "symbol"

splitSymbol :: String -> [String]
splitSymbol body = List.splitOn "symbol" body -- filter (isUpper)


--- Filter only trading pairs from the apiResponse
filterTradingPairs :: [String] -> [String]
filterTradingPairs body = tail([ filter (Chars.isUpper) x | x <- body])


-- Extract All Trading Pairs from The Binance Exchange

getTradingPairs :: IO [(String)]
getTradingPairs = do
		-- Get the latest price of all trading pairs on the Binance exchange  
                apiResponse <- getRecentBinancePrice 
                -- Extract The Response Body 
                let body = extractResponseBody apiResponse
                  --Split the response body on occurences of "symbol"
                    splitBody = splitSymbol body  
                  --Filter out all Upper case characters (All trading pairs are uppercase, none of the other characters of the response are) 
                    tradingPairs = filterTradingPairs splitBody  
                return tradingPairs


