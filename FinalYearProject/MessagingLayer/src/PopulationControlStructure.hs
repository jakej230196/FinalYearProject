{-# LANGUAGE BangPatterns #-}
module PopulationControlStructure
     (
      populate,
      timeLoop
     ) where

import GetDataTables
import DatabaseExtraction
import Data.List.Split
import DatabasePopulation
import qualified Network.HTTP.Simple as N-- Needed For The Function 'HttpBS' & 'getResponseBody'
import qualified Data.ByteString.Char8 as B8 -- Needed To Conver ByteString To String 
import Data.Typeable
--tradingPairs = GetDataTables.extractPairNames (DatabaseExtraction.extractTradingPairs)
--intervals = GetDataTables.extractIntervalNames (DatabaseExtraction.extractIntervals)
import Data.Typeable



populate :: IO ()
populate = do 
             -- get server time
             timeResponse <- N.httpBS (N.parseRequest_ "https://api.binance.com/api/v1/time") --"https://api.binance.com/api/v1/time"
             let serverTime = PopulationControlStructure.getTime timeResponse
             --print(timeResponse) 
             -- Get Trading Pairs from db
             tradingPairs <- DatabaseExtraction.extractTradingPairs
             -- get intervals from db 
             intervals <- DatabaseExtraction.extractIntervals 
             print(serverTime) 
             -- get trading pair names only
             let pairNames = ["ETHUSDT","BTCUSDT"]--GetDataTables.extractPairNames tradingPairs
                 intervalNames = ["1h"]--GetDataTables.extractIntervalNames intervals  

             cryptoLoopTest pairNames intervalNames serverTime



cryptoLoopTest :: [String] -> [String] -> Int -> IO ()
cryptoLoopTest [] _ _ = print("Trading Pair Loop Finished")
cryptoLoopTest tradingPairs intervals serverTime = do
                                                    intervalLoopTest (head tradingPairs) intervals serverTime
                                                    cryptoLoopTest (tail tradingPairs) intervals serverTime


intervalLoopTest :: String -> [String] -> Int -> IO ()
intervalLoopTest tradingPair [] _  = print("Interval Loop Finished")
intervalLoopTest tradingPair intervals serverTime = do 
                                                     -- Start From the closing time of the last entry of the table
                                                     startTime <- DatabaseExtraction.extractLastCloseTime(tradingPair ++ (head intervals))
                                                     case startTime of
                                                        Just a -> do
                                                                   timeLoop tradingPair (head intervals) a serverTime
                                                                   intervalLoopTest tradingPair (tail intervals) serverTime  

                                                        Nothing-> do
                                                                   timeLoop tradingPair (head intervals) 1502942400000 serverTime
                                                                   intervalLoopTest tradingPair (tail intervals) serverTime  


                                                 ---print("No Data In The Table")
                                                   --  timeLoop tradingPair (head intervals) startTime serverTime
                                                   --  intervalLoopTest tradingPair (tail intervals) serverTime  
                    
timeLoop :: String -> String -> Int -> Int -> IO ()
timeLoop tradingPair interval startTime endTime 
   | startTime >= endTime = print("StartTime=" ++ show startTime ++"EndTime=" ++ show endTime ++ tradingPair ++ interval ++ "Time Loop Finished")
   | otherwise = do
                   newStartTime <- DatabasePopulation.populateDB "klines" tradingPair interval startTime endTime 1000 -- get end time of last entry of response
                   timeLoop tradingPair interval (read newStartTime :: Int) endTime -- repeat
   



getTime :: N.Response B8.ByteString -> Int 
getTime apiTime = read (init (PopulationControlStructure.removeN 14 (B8.unpack(N.getResponseBody apiTime)))) :: Int

removeN :: Int -> [x] -> [x] 
removeN num lst | num <= 0 = lst
removeN _ [] = []
removeN num (_:lst) = PopulationControlStructure.removeN (num-1) lst 

extractEndTime :: [String] -> Int -> String 
extractEndTime apiRecord index 
  -- If the length is 13
    | checkLength (apiRecord!!index) == True = apiRecord!!index -- If the length of apiRecord[Index] is valid
    | otherwise = extractEndTime apiRecord (index+1) -- Recursive Calls Until We Find The End Time  


--- Function To Find The End Time Of The Last Element Of The API Response ---

getEndTime :: String -> Int 
getEndTime apiResponse = read(extractEndTime(splitString(takeLastRow(apiResponse)) ",.") 1) :: Int  -- take the last 90 characters of apiResponse
    
--- Function to remove most of an API response and then seperate the remaining section using the delimiters given in the parameters ---
{-
    Helper Function For getEndTime
    
    # Input: Api Response as a string 
    # Output: The final record of the api response
-}
-- tail
takeLastRow :: String -> String
takeLastRow apiResponse = PopulationControlStructure.removeN (getLength(apiResponse) -90) apiResponse


--- Function To Return The Length Of A List ---
{-
    # Input: Any List
    # Output: The Number Of Elements in The List 
-} 
getLength :: [x] -> Int 
getLength [] = 0
getLength (x:xs) = 1 + getLength xs


--- Function To slit up a string using the delimiters given in the parameter 
{-
    Helper Function for getEndTime
    
    # Input: A string 
    # Output: A list of the input seperated by specified characters

-}

splitString :: String -> String -> [String]
splitString sequence delimiters = Data.List.Split.splitOneOf delimiters sequence



--- Check that the value is a valid millisecond time ---
{-
    Helper Function For extractEndTime 

    # Input: A String, Taken From The API Response 
    # Output: True If Length Of The String Is 13, False Otherwise
-}
checkLength :: String -> Bool
checkLength x
    | getLength(x) == 13 = True --- Valid end time will always have a length of 13
    | otherwise = False









