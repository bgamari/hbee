{-# LANGUAGE OverloadedStrings #-}

module System.Hardware.XBee ( withXBee
                            , sendFrame
                            , recvFrame
                            , module System.Hardware.XBee.API
                            ) where

import Data.Serialize hiding (flush)
import System.Hardware.Serialport
import System.Hardware.XBee.API
import Data.List (isSuffixOf)
import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS

settings = defaultSerialSettings { commSpeed=CS9600 }

recvLine :: SerialPort -> IO (Maybe ByteString)
recvLine ser = f ""
    where f s | "\r" `BS.isSuffixOf` s  = return (Just s)
          f s  = do c <- recv ser 1
                    case c of
                         _ | BS.null c -> return Nothing
                         otherwise     -> f (s `mappend` c)

recvAck :: SerialPort -> Int -> IO ()
recvAck ser 100 = fail "No ACK recieved after 100 packets. Giving up."
recvAck ser n = do
    f <- recvFrame ser
    case f of
         Right (Frame (ATResponse {apiStatus=0})) -> return ()
         _ -> recvAck ser (n+1)

enterAPI :: SerialPort -> IO ()
enterAPI ser = do
    flush ser
    send ser "+++"
    a <- recvLine ser
    case a of
         -- Confirm that we're already in API mode
         Nothing     -> do sendFrame ser $ Frame $ ATCommand 1 "AP" BS.empty
                           recvAck ser 0
         -- Enable API mode
         Just "OK\r" -> do send ser "ATAP 1\r"
                           Just a <- recvLine ser
                           send ser "ATAP\r"
                           Just a <- recvLine ser
                           print a
                           send ser "ATCN\r"
                           Just a <- recvLine ser
                           flush ser
         Just x      -> do sendFrame ser $ Frame $ ATCommand 1 "AP" BS.empty
                           recvAck ser 0

sendFrame :: SerialPort -> Frame -> IO ()
sendFrame ser frame = let a = encode frame
                      in send ser a >> return ()

recvFrame :: SerialPort -> IO (Either String Frame)
recvFrame ser = f $ runGetPartial (get :: Get Frame)
    where f cont = do a <- recv ser 20
                      if BS.null a
                         then f cont
                         else case cont a of
                           Fail err      -> return (Left err)
                           Partial cont' -> f cont'
                           Done r rest   -> return (Right r)

withXBee :: FilePath -> (SerialPort -> IO ()) -> IO ()
withXBee device f = withSerial device settings (\ser -> do enterAPI ser
                                                           f ser)

