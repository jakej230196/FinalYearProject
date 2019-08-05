--{-# LANGUAGE OverloadedStrings #-}

module ParseApiResponse
    ( 
      parseResponseString,
      CandleStick
    ) where


import Data.Strings as S
import qualified Data.ByteString.Char8 as B8 -- Allows decoding of the API Response object
--import Text.Show
import Data.List.Split
--import Data.Typeable
import Data.List
import qualified Data.Strings as Str


--data CandleStick = CandleStick {startTime :: Int, open :: Float, high :: Float, low :: Float, close :: Float, closeTime :: Int , Float Float Int} deriving (Show)


data CandleStick = CandleStick
   { openTime :: Int
   , open :: Float
   , high :: Float
   , low :: Float
   , close :: Float
   , volume:: Float
   , closeTime :: Int
   , qav :: Float
   , trades :: Int
   , buyBAV :: Float
   , buyQAV :: Float
   , exchange :: Float
   } deriving (Show,Read)


-- Convert bytestring to string & remove unneeded characters

toString :: B8.ByteString -> String
toString responseBody = (S.strReplace "[(" "" (S.strReplace "\"" ""(S.strReplace "))" ")]" (S.strReplace "((" "[(" (S.strReplace "]" ")" (S.strReplace "[" "(" (B8.unpack responseBody)))))))

-- Need to replace the first elements opentime field, first element of API response includes characters "[("


--- Prepare data for parsing as a CandleStick object  (Using a custom data type) 

parseElementCandleSticks :: String -> String -> String
parseElementCandleSticks element exchange = parsedElement 
                                  where splitElement = splitOn "," element --- Split into a list
                                        removedIgnoreVal = init (splitElement) --- remove the last element of the list (The binance ignore val)
                                        openTime = "CandleStick { openTime = " ++ removedIgnoreVal!!0 ++ "," -- format the open time
                                        openPrice = openTime ++ "open = " ++ removedIgnoreVal!!1 ++ ","  -- format and append the open price to opentime
                                        highPrice = openPrice ++ "high = " ++ removedIgnoreVal!!2 ++ "," -- format and append high price (and so on)
                                        lowPrice = highPrice ++ "low = " ++ removedIgnoreVal!!3 ++ ","
                                        closePrice = lowPrice ++ "close = " ++ removedIgnoreVal!!4 ++ ","
                                        volume = closePrice ++ "volume = " ++ removedIgnoreVal!!5 ++ ","
                                        closeTime = volume ++ "closeTime = " ++ removedIgnoreVal!!6 ++ ","
                                        qav = closeTime ++ "qav = " ++ removedIgnoreVal!!7 ++ ","
                                        trades = qav ++ "trades = " ++ removedIgnoreVal!!8 ++ ","
                                        buyBAV = trades ++ "buyBAV = " ++ removedIgnoreVal!!9 ++ ","
                                        buyQAV = buyBAV ++ "buyQAV = " ++ removedIgnoreVal!!10 ++ ","   
                                        parsedElement = buyQAV ++ "exchange = " ++ exchange ++ "}"


--- List comprehension to prepare all API response elements for parsng to CandleStick Objects

parseElementsCandleSticks elements exchange = [(parseElementCandleSticks element exchange)|element <- elements ]


parseResponseCandleSticks :: B8.ByteString -> String -> [(CandleStick)]
parseResponseCandleSticks body exchange = parsedBody where
                                            bodyString = ParseApiResponse.toString body -- Convert Bytestring to string & remove/replace characters
                                            bodyEndBy = endBy "),(" bodyString -- seperate response into individual elements
                                            parsedBody = read ("[" ++ (intercalate "," (parseElementsCandleSticks bodyEndBy exchange)) ++ "]") :: [(CandleStick)] -- parse as list of candlesticks
                               

--- List comprehension to prepare all API response elements for parsng to CandleStick Objects

parseElementsString elements exchange = [(parseElementString element exchange)|element <- elements ]


parseResponseString :: B8.ByteString -> String -> String
parseResponseString body exchange = parsedBody where
                                            bodyString = ParseApiResponse.toString body -- Convert Bytestring to string & remove/replace characters
                                            bodyEndBy = endBy "),(" bodyString -- seperate response into individual elements
                                            parsedBody = ("[" ++ (intercalate "," (parseElementsString bodyEndBy exchange)) ++ "]") -- parse as list of candlesticks
                               

--- Prepare data for parsing as a CandleStick object -- MYSQL library does not support inserting candlestikc objects so this parsed as a string

parseElementString :: String -> String -> String
parseElementString element exchange = parsedElement 
                                  where splitElement = splitOn "," element --- Split into a list
                                        removedIgnoreVal = init (splitElement) --- remove the last element of the list (The binance ignore val)
                                        openTime = "(" ++ removedIgnoreVal!!0 ++ "," -- format the open time
                                        openPrice = openTime ++ removedIgnoreVal!!1 ++ ","  -- format and append the open price to opentime
                                        highPrice = openPrice ++ removedIgnoreVal!!2 ++ "," -- format and append high price (and so on)
                                        lowPrice = highPrice ++ removedIgnoreVal!!3 ++ ","
                                        closePrice = lowPrice ++ removedIgnoreVal!!4 ++ ","
                                        volume = closePrice ++ removedIgnoreVal!!5 ++ ","
                                        closeTime = volume ++ removedIgnoreVal!!6 ++ ","
                                        qav = closeTime ++ removedIgnoreVal!!7 ++ ","
                                        trades = qav ++ removedIgnoreVal!!8 ++ ","
                                        buyBAV = trades ++ removedIgnoreVal!!9 ++ ","
                                        buyQAV = buyBAV ++ removedIgnoreVal!!10 ++ ","   
                                        parsedElement = buyQAV ++ exchange ++ ")"






                    


{-
getLength :: [x] -> Int 
getLength [] = 0
getLength (x:xs) = 1 + getLength xs

splitString :: String -> String -> [String]
splitString sequence delimiters = init(Data.List.Split.splitOneOf delimiters sequence)
-}
