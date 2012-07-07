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

settings = defaultSerialSettings { block=Block 100, commSpeed=CS9600 }

recvLine :: SerialPort -> IO (Maybe ByteString)
recvLine ser = f ""
        where f s | "\r" `BS.isSuffixOf` s  = return (Just s)
              f s  = do c <- recv ser 1
                        case c of
                             _ | BS.null c -> return Nothing
                             otherwise     -> f (s `mappend` c)

enterAPI :: SerialPort -> IO ()
enterAPI ser = do flush ser
                  send ser "+++"
                  a <- recvLine ser
                  case a of
                       -- Assume we're already in API mode
                       Nothing     -> return ()
                       -- Enable API mode
                       Just "OK\r" -> do send ser "ATAP 1\r"
                                         Just a <- recvLine ser
                                         send ser "ATAP\r"
                                         Just a <- recvLine ser
                                         print a
                                         send ser "ATCN\r"
                                         Just a <- recvLine ser
                                         flush ser
                       Just x      -> return () --fail $ "Unknown response: "++show x

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

