import Data.Maybe (maybeToList)
import Data.List (isSuffixOf)
import System.Hardware.Serialport
import System.Hardware.XBee.API
import Data.Serialize
import qualified Data.ByteString as BS

import Text.PrettyPrint.HughesPJ
import Numeric (showHex)

settings = defaultSerialSettings { timeout=10 }

recvLine :: SerialPort -> IO String
recvLine ser = f ""
        where f s | "\r" `isSuffixOf` s  = return s
              f s                        = do c <- recvChar ser
                                              f (s ++ maybeToList c)

main = withSerial "/dev/ttyUSB0" settings test
test ser = do sendString ser "+++"
              a <- recvLine ser
              print a
              sendString ser "ATAP1\r\n"
              a <- recvLine ser
              print a
              let cmd = (ATCommand { apiFrameId=0x0
                                   , apiATCommand="ATND"
                                   , apiParam=BS.empty })
              sendFrame ser $ Frame cmd
              f <- recvFrame ser
              print f

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
