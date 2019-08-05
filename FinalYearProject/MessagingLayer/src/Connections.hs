module Connections
    ( 
      prepSocket,
      receiveConn,
      dataStream,
      readRequest,
      closeStream,
      closeConnection, 
      purge
    ) where

import qualified Network.HTTP.Server as Server -- For request objects
import qualified Network.Socket as Socket -- Needed For Socket actions
import qualified Network.HTTP.Listen as Listen -- Needed For Connection Objects
import qualified Network.Stream as NS -- for connection errors
import qualified Data.Either as Either -- for Result type

--- Result types: Either a network streaming error or Request/Response String

type Result a = Either.Either NS.ConnError a

--- Initialize socket to accept connections

prepSocket :: Int -> IO Socket.Socket
prepSocket port = Listen.prepareSocket port

--- Await client connections

receiveConn :: Socket.Socket -> IO Listen.Connection
receiveConn socket = Listen.acceptConnection socket

--- Open a data stream over a given connection

dataStream :: Listen.Connection -> IO (Listen.Stream String)
dataStream conn = Listen.openStream conn  

--- Read Request From Data stream

readRequest :: Listen.Stream String -> IO (Result (Server.Request String))
readRequest stream = Listen.receiveRequest stream

--- close client stream
closeStream :: Listen.Stream String -> IO ()
closeStream stream = Listen.closeStream stream

--- close client Connection
closeConnection :: Listen.Connection -> IO ()
closeConnection conn = Listen.closeConnection conn


--- Close all connections

purge :: Listen.Stream String -> Listen.Connection -> Socket.Socket -> IO ()
purge stream conn socket = do 
                             Listen.closeStream stream 
                             Listen.closeConnection conn
                             Socket.close socket
                           
