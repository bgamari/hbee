import Data.Serialize
import qualified Data.ByteString as BS
import System.Hardware.XBee
import Control.Monad
import Debug.Trace
import Text.Printf
import Data.List (intercalate)


main = withXBee "/dev/ttyUSB0" test
test ser = do let cmd = (ATCommand { apiFrameId=0x52
                                   , apiATCommand="ND"
                                   , apiParam=BS.empty })
              print cmd
              print $ formatBytes $ encode $ Frame cmd
              sendFrame ser $ Frame cmd
              forever (recvFrame ser >>= print)

formatBytes :: BS.ByteString -> String
formatBytes = intercalate " " . map (printf "%02x") . BS.unpack

