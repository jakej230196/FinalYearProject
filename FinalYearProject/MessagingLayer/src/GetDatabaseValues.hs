{-# LANGUAGE OverloadedStrings #-}

--- ############ Module will be obsolete once messaging layer is equipped with all relevent SQL interactions ########-----

module GetDatabaseValues
    (
      constructDbString,
      extractTradingPairID,
      extractDbExID,
      extractIntervalID,
      extractEndTime, 
      splitString,
      checkLength,
      getLength,
      getEndTime,
      takeLastRow,
      removeN,
      extractTradingPairIDString
      
       
    ) where

import Network.HTTP.Simple as H --Imports functions for making API request and response objects
import qualified Data.ByteString.Char8 as B8 -- Allows decoding of the API Response object
import qualified Data.Map as Map
import Data.Maybe -- Needed To Convert Maybe Values & Just Values  -> Ordinary Value
import Data.List.Split
import Network.HTTP.Conduit -- Needed to allow API Request Objects To Be Decomposed


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

--- Loop/Recursion control structure testing variables ---

testInitialStartTime = 100
testStartTime = testInitialStartTime
testEndTime = 200

testTradingPairs = ["BTCUSDT","ETHUSDT","LTCUSDT"]
testIntervals = ["1m","2m","3m","4m","5m"]

--- Just using BTCUSDT and ETHUSDT for Testing ---

-- Dictionary That Stores A Trading Pair And The Trading Pairs Primary Key Id. The Trading Pair is The Key And The Primary Key Is The Value 
tradingPairs = Map.fromList [("BTCUSDT",1),("ETHUSDT",2)] 
 
-- Create a Dictionary Intervals Where The Key Value Is A Binance Timeframe And The Value Is The Corresponding Database Interval

intervals = Map.fromList [("1m",1),("3m",2),("5m",3),("15m",4),("30m",5),("1h",6),("2h",7),("4h",8),("6h",9),("12h",10),
                          ("1d",11),("3d",12),("1w",13),("1M",14)]

-- ("I SAID \"HELLO\"")
-- Dictionary That Maps Exchange Names To Their Unique DataBase ID --

exchanges = Map.fromList [("binance",1),("bittrex",2)]

-- Initial Start Time for testing -- 
initialStartTime = 1549032470000

-- EndTime for testing ---
endTime = 1549810070000  



--- Function to extract and concatenate database values for { TradingPairID, ExchangeID, IntervalID }---
{-

  # Input is the API Request Object (Request) and Output is the concatenation of all the values (String) in the correct order for database insertion 
  
  # Function extractIntervalID Returns The Database Equivilent of the exchange interval (String) e.g a one min interval on the binance exchange is '1m' and in the database it is '1Min'
  # Function extractTradingPairID Returns 
  # Function extractDbExID Returns The Database Unique ID of the exchange to which the api request was made (String)
  
-}
 

constructDbStringTEST :: Request -> String 
constructDbStringTEST apiRequest =  "," ++ extractTradingPairID apiRequest ++ "," ++ extractDbExID apiRequest ++ "," ++ show(extractIntervalID apiRequest) ++ ")" {- \" Is an Escape Character, allows interval to be wrapped with ""-}


constructDbString :: Request -> String 
constructDbString apiRequest =  "," ++ extractTradingPairID apiRequest ++ "," ++ extractDbExID apiRequest ++ "," ++ show(extractIntervalID apiRequest) ++ ")" {- \" Is an Escape Character, allows interval to be wrapped with ""-}


--- Function To Extract Database Primary Key For The Exchange The Request Was Made To --- 
{-
    Currently only taking data from binance which has the PK value: 1. 
    Statically typed for now, make it dynamic once other exchanges are used.
    
    
  #  Inputs The API Request Object and Outputs The Exchange ID 
    
  #  Dictionary Search Returns A 'Just (Value)' so we have to convert it to an Integer using 'fromJust' 
  #  We search the dictionary using 'Map.Lookup', the key we are searching for is obtained by unpacking the bytestring that holds the adress of the API host and using '.' as a delimiter to seperate the string 
  #  the host path is typically 'api.XYZ.com' so we can then take element 2 (Index 1) of the List which gives us the exchanges name. 
  #  The 'show' function converts an interger type to a string
-}

extractDbExID :: Request -> String
--extractDbExID apiRequest = show(fromJust(Map.lookup (Data.List.Split.splitOn "."(S8.unpack(host apiRequest)) !! 1) exchanges ))
extractDbExID apiRequest =  show(fromJust(Map.lookup((splitString (B8.unpack(host apiRequest)) ".") !!1 ) exchanges))



--- Function To Extract The Primary Key For The Trading Pair Of The API Request ---
{-
    Uses The Dictionary 'tradingPairs' To Obtain The PK Value Using The Name Of The Trading Pair In The Request.
    Currently statically typed for testing but should be made dynamic later
    
  #  Inputs  The API Request Object And Outputs The Primary Key of The Trading Pair
    
  #  Dictionary Search Returns A 'Just (Value)' so we have to convert it to an Integer using 'fromJust'
  #  Dictionary is searched using 'Map.lookup' using the key obtained from unpacking the request object queryString (Bytestring) and then splitting the string using 'SplitOneOf' " = & "
  #  which separates the string if either of the characters occur. The format of the request query string means that the name of the trading pair will always be the second element of the resulting list (Index 1)
  # The 'show' function converts an interget type to a string
    
-}


extractTradingPairID :: Request -> String 
extractTradingPairID apiRequest =  show(fromJust(Map.lookup((splitString (B8.unpack(queryString apiRequest)) "=&") !!1 ) tradingPairs))


extractTradingPairIDString :: String -> Int
extractTradingPairIDString symbol = fromJust(Map.lookup(symbol) tradingPairs)

--- Function To Extract The TimeFrame Interval Of The API Request ---
{-
    Uses the Dictionary 'intervals' to Obtain The Correct Database Value For The Interval In The API Request. 
    Interval Value Withtin The API Request Is Used to Search the Dictionary.
    
    
  #  Inputs  The API Request Object And Outputs The Batabase value equivilent of the binance interval tag
    
  #  Dictionary Search Returns A 'Just (Value)' so we have to convert it to a string using 'fromJust'
  #  Dictionary is searched using 'Map.lookup' using the key obtained from unpacking the request object queryString (Bytestring) and then splitting the string using 'SplitOneOf' " = & "
  #  which separates the string if either of the characters occur. The format of the request query string means that the Interval tag will always be the fourth element of the resulting list (Index 3)
  
    
-}

extractIntervalID :: Request -> Int
extractIntervalID apiRequest = fromJust(Map.lookup((splitString (B8.unpack(queryString apiRequest)) "=&") !!3 ) intervals)

--- Function To Extract The End Time Of The Last Entry Of The API Response

-- Helper Function for getEndTime 

extractEndTime :: [String] -> Int -> String 
extractEndTime apiRecord index 
  -- If the length is 13
    | checkLength (apiRecord!!index) == True = apiRecord!!index -- If the length of apiRecord[Index] is valid
    | otherwise = extractEndTime apiRecord (index+1) -- Recursive Calls Until We Find The End Time  


--- Function To Find The End Time Of The Last Element Of The API Response ---

{-

-}

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
takeLastRow apiResponse = removeN (getLength(apiResponse) -90) apiResponse


--- Function To Remove Elements From a List ---
{-
    # Input: List Index, List
    # Output: List With index removed 
-}

removeN :: Int -> [x] -> [x] 
removeN num lst | num <= 0 = lst
removeN _ [] = []
removeN num (_:lst) = removeN (num-1) lst 






























