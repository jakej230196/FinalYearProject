{-# LANGUAGE OverloadedStrings #-}
module GetDataTables
    ( 
      getDataTables,
      extractPairNames,
      extractIntervalNames
    ) where

import qualified GetTradingPairs as GTP
import qualified DatabaseExtraction as DE
import Data.Typeable ---TEMP


-- Create a list of all TradingPair and interval combinations

getDataTables :: IO [[String]]
getDataTables = do
		-- Extract all trading pairs from the database
                tradingPairs <- DE.extractTradingPairs
                -- Extract all intervals from the database
                intervals <- DE.extractIntervals
                -- extract the names from the trading pair objects
                let pairNames = extractPairNames tradingPairs
                    -- Extract names from interval objects
                    intervalNames = extractIntervalNames intervals
                    -- Cocat trading pairs and intervals
                    tables = [tradingPairsIntervalLoop tradingPair intervalNames | tradingPair <- pairNames]

                --print (tables)
                --print (typeOf tables)
                return tables



--- Extract tradingPair names from a list of tradingPair objects
extractPairNames :: [DE.TradingPair] -> [String]
extractPairNames pairs = ([DE.getPairName y | y <- pairs])
                       
--- Extract Interval names form interval objects
extractIntervalNames :: [DE.Interval] -> [String]
extractIntervalNames intervals = ([DE.getIntervalName y | y <- intervals])

--- Create a list of all trading pair and interval combinations
tradingPairsIntervalLoop tradingPair intervals = [tradingPairsConcatLoop tradingPair interval [] | interval <- intervals ]
tradingPairsConcatLoop tradingPair interval acc  = tradingPair ++ interval



 
{-

main = do
        -- Get exchange Info
        apiResponse <- httpBS  "https://api.binance.com/api/v3/ticker/price"
       -- print(Data.List.length (S8.unpack(getResponseBody apiResponse))) 
        let body =  (S8.unpack(getResponseBody apiResponse))
        let x = splitOn "symbol" body -- filter (isUpper)
        let tradingPairs = tail(["ggg" ++ filter (isUpper) z ++ "ggg"| z <- x])
        print tradingPairs
       -- let pairsIntervals = ([ x ++ show(y) | y <- testIntervals])
       -- print(pairsIntervals) 
       -- let l = [ tradingPairsIntervalLoop tradingPair | tradingPair <- tradingPairs]
       -- print l  
       -- print(typeOf(l))
      --  print(l!!0 !!0)

--- gETS Trading Pairs + Intervals
       -- let pairsIntervals = ([ x ++ show(y) | y <- testIntervals])
       -- print(pairsIntervals) 
       -- let l = [ tradingPairsIntervalLoop tradingPair | tradingPair <- tradingPairs]
       -- print l  
       -- print(typeOf(l))
      --  print(l!!0 !!0) 
-}
