------------------------------------------------------------------------------
--- A simple "addition" server to test the Socket library with time limits
--- on socket connections.
---
--- @author Michael Hanus
--- @version November 2020
------------------------------------------------------------------------------

import System.IO
import Network.Socket

-- Choose a free port number:
portnr :: Int
portnr = 65502

sendTo :: String -> String -> IO ()
sendTo host msg = do
  h <- connectToSocket host portnr
  hPutStr h msg
  hClose h

stopServer :: String -> IO ()
stopServer host = sendTo host "TERMINATE\n"


-- An "addition" server:
addServer :: IO ()
addServer = do
  socket <- listenOn portnr
  putStrLn $ "Serving port: " ++ show portnr
  addServeSocket socket

addServeSocket :: Socket -> IO ()
addServeSocket socket = do
  conn <- waitForSocketAccept socket 1000
  addServeSocketTest socket conn

addServeSocketTest :: Socket -> Maybe (String,Handle) -> IO ()
addServeSocketTest socket Nothing = do
  putStrLn "Timeout"
  addServeSocket socket
addServeSocketTest socket (Just (chost,stream)) = do
  putStrLn $ "Connection from "++chost
  serverLoop stream
 where
   serverLoop h = do
     l1 <- hGetLine h
     if l1 == "TERMINATE"
      then do hClose h
              close socket
      else do l2 <- hGetLine h
              hPutStrLn h (show ((read l1 :: Int) + (read l2 :: Int)))
              hClose h
              addServeSocket socket

addClient :: String -> Int -> Int -> IO ()
addClient host x y = do
  h <- connectToSocket host portnr
  hPutStr h (unlines (map show [x,y]))
  hFlush h
  answer <- hGetLine h
  putStrLn $ "Answer: "++answer
  hClose h

{-
Test with PAKCS:

:fork addServer
addClient "localhost" 3 4
stopServer "localhost"

-}