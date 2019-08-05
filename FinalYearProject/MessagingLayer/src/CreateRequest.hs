{-# LANGUAGE OverloadedStrings #-}
module CreateRequest
    ( 
      constructRequest
    ) where


                         
--- Create the api query string ----

{-
	Base endpoint is: https://api.binance.com/api/v1/
	req: The api request type (Candlestick request = Klines)
	interval: The api request candlestick interval
	limit: How many entries to return. Default = 500, Max = 1000
	
-}
constructRequest :: String -> String -> String -> Int -> Int -> Int -> String --function declaration
constructRequest req symbol interval startTime endTime limit = 
                                                          "https://api.binance.com/api/v1/" ++ req ++
                                                          "?symbol=" ++ symbol ++
                                                          "&interval=" ++ interval ++ 
                                                          "&startTime=" ++ (show startTime) ++
                                                          "&endTime=" ++ (show endTime) ++
                                                          "&limit=" ++ (show limit)

