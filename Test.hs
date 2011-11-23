import Data.List (isSuffixOf)
import System.Hardware.Serialport
import System.Hardware.XBee.API
import Data.Serialize hiding (flush)
import qualified Data.ByteString as BS

import Debug.Trace
import Text.Printf
import Data.List (intercalate)

settings = defaultSerialSettings { timeout=50 }

recvLine :: SerialPort -> IO (Maybe String)
recvLine ser = f ""
        where f s | "\r" `isSuffixOf` s  = return (Just s)
              f s  = do c <- recvChar ser
                        case c of
                             Nothing -> return Nothing
                             Just c' -> f (s++[c'])

main = withSerial "/dev/ttyUSB0" settings test
test ser = do enterAPI ser
              let cmd = (ATCommand { apiFrameId=0x52
                                   , apiATCommand="ND"
                                   , apiParam=BS.empty })
              print cmd
              print $ formatBytes $ encode $ Frame cmd
              flush ser
              sendFrame ser $ Frame cmd
              f <- recvFrame ser
              print f

enterAPI :: SerialPort -> IO ()
enterAPI ser = do sendString ser "+++"
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
                          traceShow a $ return ()
                          case cont (packToByteString a) of
                               Fail err      -> fail err
                               Partial cont' -> f cont'
                               Done r rest   -> return r

formatBytes :: BS.ByteString -> String
formatBytes = intercalate " " . map (printf "%02x") . BS.unpack

