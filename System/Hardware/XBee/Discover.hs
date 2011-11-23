module System.Hardware.XBee.Discover (discoverNodes) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Serialize
import System.Hardware.XBee
import System.Hardware.XBee.Utils
import System.Hardware.Serialport

discoverNodes :: SerialPort -> IO [NodeDiscResp]
discoverNodes ser =
        do let cmd = ATCommand { apiFrameId=0x52
                               , apiATCommand="ND"
                               , apiParam=BS.empty }
           sendFrame ser $ Frame cmd
           let collect :: [NodeDiscResp] -> IO [NodeDiscResp]
               collect nodes = do Frame f <- recvFrame ser
                                  let d = apiCmdData f
                                  if BS.null d
                                     then return nodes
                                     else case decode d of 
                                               Left err -> collect nodes
                                               Right n  -> collect (n:nodes)
           collect []

data NodeDiscResp = NodeDiscResp { ndAddress :: Word16
                                 , ndSerial :: Word64
                                 , ndRssi :: Word8
                                 , ndIdent :: String
                                 } deriving (Show, Eq)

instance Serialize NodeDiscResp where
        put _ = error "Put not implemented"
        get =
                do addr <- getWord16be
                   serHi <- getWord32be
                   serLo <- getWord32be
                   rssi <- getWord8
                   ident <- getRemainingBytes
                   return NodeDiscResp { ndAddress = addr
                                       , ndSerial = fromIntegral serHi `shift` 32 .|. fromIntegral serLo
                                       , ndRssi = rssi
                                       , ndIdent = unpackToString ident }

