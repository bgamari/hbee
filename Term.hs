import Data.Serialize
import qualified Data.ByteString as BS
import System.Hardware.XBee
import System.Hardware.XBee.Utils
import System.Hardware.Serialport
import Control.Monad
import Control.Concurrent (forkIO)

main = withXBee "/dev/ttyUSB0" test
test ser = do forkIO (forever $ reader ser)
              forever (getLine >>= (\s->send ser (s++"\r\n")))

reader ser = do recvFrame ser >>= print

send :: SerialPort -> String -> IO ()
send ser s = let cmd = TransmitRequest { apiFrameId=0x1
                                       , apiDest=ShortAddr 2
                                       , apiOptions=0
                                       , apiData=packToByteString s }
             in sendFrame ser $ Frame cmd

