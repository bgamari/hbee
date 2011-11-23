module System.Hardware.XBee ( withXBee
                            , sendFrame
                            , recvFrame
                            , module System.Hardware.XBee.API
                            ) where

import Data.Serialize hiding (flush)
import System.Hardware.Serialport
import System.Hardware.XBee.API
import Data.List (isSuffixOf)

settings = defaultSerialSettings { timeout=50 }

recvLine :: SerialPort -> IO (Maybe String)
recvLine ser = f ""
        where f s | "\r" `isSuffixOf` s  = return (Just s)
              f s  = do c <- recvChar ser
                        case c of
                             Nothing -> return Nothing
                             Just c' -> f (s++[c'])

enterAPI :: SerialPort -> IO ()
enterAPI ser = do flush ser
                  sendString ser "+++"
                  a <- recvLine ser
                  case a of
                       -- Assume we're already in API mode
                       Nothing     -> return ()
                       -- Enable API mode
                       Just "OK\r" -> do sendString ser "ATAP 1\r"
                                         Just a <- recvLine ser
                                         sendString ser "ATAP\r"
                                         Just a <- recvLine ser
                                         print a
                                         sendString ser "ATCN\r"
                                         Just a <- recvLine ser
                                         flush ser
                       Just _      -> fail "Unknown response"

sendFrame :: SerialPort -> Frame -> IO ()
sendFrame ser frame = let a = encode frame
                      in sendString ser (unpackToString a)

recvFrame :: SerialPort -> IO Frame
recvFrame ser = f $ runGetPartial (get :: Get Frame)
        where f cont = do a <- recvString ser
                          case cont (packToByteString a) of
                               Fail err      -> fail err
                               Partial cont' -> f cont'
                               Done r rest   -> return r

withXBee :: FilePath -> (SerialPort -> IO ()) -> IO ()
withXBee device f = withSerial device settings (\ser -> do enterAPI ser
                                                           f ser)

