module System.Hardware.XBee.Discover (discoverNodes) where

import qualified Data.ByteString as BS
import Data.Word
import Data.Bits
import Data.Serialize
import System.Hardware.XBee
import System.Hardware.XBee.Utils
import System.Hardware.Serialport

data RespFrame = FinishedFrame | NodeFrame NodeDiscResp | EmptyFrame

discoverNodes :: SerialPort -> IO [NodeDiscResp]
discoverNodes ser =
        do let cmd = ATCommand { apiFrameId=0x52
                               , apiATCommand="ND"
                               , apiParam=BS.empty }
           sendFrame ser $ Frame cmd
           let nodeFromFrame :: Either String Frame -> RespFrame
               nodeFromFrame (Left _) = EmptyFrame
               nodeFromFrame (Right (Frame (ATResponse { apiCmdData=d })) )
                        | BS.null d  = FinishedFrame
                        | otherwise  = case decode d of
                                            Left err -> EmptyFrame
                                            Right n  -> NodeFrame n
               nodeFromFrame (Right _) = EmptyFrame

               collect :: [NodeDiscResp] -> IO [NodeDiscResp]
               collect nodes =
                        do r <- recvFrame ser
                           case nodeFromFrame r of
                                FinishedFrame  -> return nodes
                                NodeFrame n    -> collect (n:nodes)
                                EmptyFrame     -> collect nodes
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

