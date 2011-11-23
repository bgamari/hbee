import Data.List (isSuffixOf)
import System.Hardware.Serialport
import System.Hardware.XBee.API
import Data.Serialize
import qualified Data.ByteString as BS

import Text.PrettyPrint.HughesPJ
import Numeric (showHex)

settings = defaultSerialSettings { timeout=10 }

recvLine :: SerialPort -> IO (Maybe String)
recvLine ser = f ""
        where f s | "\r" `isSuffixOf` s  = return (Just s)
              f s  = do c <- recvChar ser
                        case c of
                             Nothing -> return Nothing
                             Just c' -> f (s++[c'])

main = withSerial "/dev/ttyUSB0" settings test
test ser = do enterAPI ser
              let cmd = (ATCommand { apiFrameId=0x0
                                   , apiATCommand="ATND"
                                   , apiParam=BS.empty })
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
                                         print "In API mode"
                       Just _      -> fail "Unknown response"

sendFrame :: SerialPort -> Frame -> IO ()
sendFrame ser frame = let a = encode frame
                      in print (byteString a) >> sendString ser (unpackToString a)

recvFrame :: SerialPort -> IO Frame
recvFrame ser = f $ runGetPartial (get :: Get Frame)
        where f cont = do a <- recvString ser
                          case cont (packToByteString a) of
                               Fail err     -> fail err
                               Partial conf -> f cont
                               Done r rest  -> return r

byteString :: BS.ByteString -> Doc
byteString = hsep . map (\x->text $ showHex x "") . BS.unpack
