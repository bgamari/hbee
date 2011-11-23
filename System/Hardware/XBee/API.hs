module System.Hardware.XBee.API ( Address(..)
                                , TxStatus(..)
                                , APICmd(..)
                                , Frame(..)
                                ) where

import Data.List (foldl1')
import Data.Char (ord)
import Data.Word
import Data.Bits
import qualified Data.ByteString as BS
import Data.Serialize
import Data.Serialize.Get (getBytes)
import Control.Monad (when)
import Text.Printf
import System.Hardware.XBee.Utils

data Address = LongAddr Word64
             | ShortAddr Word16
             deriving (Show, Eq)

data TxStatus = TxSuccess
              | TxNoAck
              | TxCCAFailure
              | TxPurged
              deriving (Show, Eq)

data APICmd -- * Sent from device to host
            = ModemStatus { apiStatus :: Word8 }
            | ATResponse { apiFrameId :: Word8
                         , apiATCommand :: String
                         , apiStatus :: Word8
                         , apiCmdData :: BS.ByteString }
            | RemoteATResponse { apiFrameId :: Word8
                               , apiAddr :: Address
                               , apiATCommand :: String
                               , apiStatus :: Word8 
                               , apiCmdData :: BS.ByteString}
            | TransmitStatus { apiFrameId :: Word8
                             , apiTxStatus :: TxStatus }
            | Receive { apiSource :: Address
                      , apiRSSI :: Word8
                      , apiOptions :: Word8
                      , apiData :: BS.ByteString }
            | IOData { apiSource :: Address
                     , apiRSSI :: Word8
                     , apiOptions :: Word8
                     , apiDChannels :: [(Int, Bool)]
                     , apiAChannels :: [(Int, Word16)] }

            -- * Sent from host to device
            | ATCommand { apiFrameId :: Word8
                        , apiATCommand :: String
                        , apiParam :: BS.ByteString }
            | ATQueueCommand { apiFrameId :: Word8
                             , apiATCommand :: String
                             , apiParam :: BS.ByteString }
            | RemoteATCommand { apiFrameId :: Word8
                              , apiDest :: Address
                              , apiOptions :: Word8
                              , apiATCommand :: String
                              , apiParam :: BS.ByteString }
            | TransmitRequest { apiFrameId :: Word8
                              , apiDest :: Address
                              , apiOptions :: Word8
                              , apiData :: BS.ByteString }
            deriving (Show, Eq)

instance Serialize APICmd where
        get = 
                do cmdid <- get :: Get Word8
                   case cmdid of
                        0x8a -> do statusByte <- get
                                   return $ ModemStatus statusByte
                        0x88 -> do frameId <- get
                                   cmd <- getBytes 2
                                   status <- get
                                   cmdData <- getRemainingBytes
                                   return $ ATResponse frameId (unpackToString cmd) status cmdData
                        0x97 -> do frameId <- get
                                   addr64 <- get :: Get Word64
                                   addr16 <- get :: Get Word16
                                   cmd <- getBytes 2
                                   status <- get
                                   cmdData <- getRemainingBytes
                                   let addr = if addr64 /= 0 then LongAddr addr64
                                                             else ShortAddr addr16
                                   return $ RemoteATResponse frameId addr (unpackToString cmd) status cmdData
                        0x89 -> do frameId <- get
                                   s <- get :: Get Word8
                                   let status = case s of
                                                     0 -> TxSuccess
                                                     1 -> TxNoAck
                                                     2 -> TxCCAFailure
                                                     3 -> TxPurged
                                   return $ TransmitStatus frameId status
                        0x80 -> do source <- get
                                   rssi <- get
                                   options <- get
                                   d <- getRemainingBytes
                                   return $ Receive (LongAddr source) rssi options d
                        0x81 -> do source <- get
                                   rssi <- get
                                   options <- get
                                   d <- getRemainingBytes
                                   return $ Receive (ShortAddr source) rssi options d
                        0x82 -> do source <- get
                                   getIOData (LongAddr source)
                        0x83 -> do source <- get
                                   getIOData (ShortAddr source)
                        x    -> fail $ printf "Unknown API command 0x%02x" x

                where getIOData source = do
                                   rssi <- get
                                   options <- get
                                   nSamp <- get :: Get Word8
                                   chInd <- get :: Get Word16
                                   let activeD = filter (testBit chInd) [0..8]
                                       activeA = filter (\i->testBit chInd (i+9)) [0..5]
                                   --when (length activeA /= fromIntegral nSamp) (fail "Inconsistent sample count") -- TODO: Not sure why this doesn't hold
                                   dValues <- if null activeD
                                                 then return []
                                                 else do dCh <- get :: Get Word16
                                                         return $ zip activeD (map (testBit dCh) activeD)  
                                   aChs <- mapM (\_ -> get :: Get Word16) [0..nSamp]
                                   let aValues = zip activeA aChs
                                   return $ IOData { apiRSSI = rssi
                                                   , apiOptions = options
                                                   , apiSource = source
                                                   , apiDChannels = dValues
                                                   , apiAChannels = aValues }


        put (ATCommand frameId cmd param) =
                do put (0x08 :: Word8)
                   put frameId
                   putByteString (packToByteString cmd)
                   putByteString param

        put (ATQueueCommand frameId cmd param) =
                do put (0x09 :: Word8)
                   put frameId
                   putByteString (packToByteString cmd)
                   putByteString param

        put (RemoteATCommand frameId (ShortAddr dest) options cmd param) =
                do putWord8 0x17
                   putWord8 frameId
                   putWord64be 0
                   putWord16be dest
                   putWord8 options
                   putByteString (packToByteString cmd)
                   putByteString param

        put (RemoteATCommand frameId (LongAddr dest) options cmd param) =
                do putWord8 0x17
                   putWord8 frameId
                   putWord64be dest
                   putWord16be 0xfffe
                   putWord8 options
                   putByteString (packToByteString cmd)
                   putByteString param

        put (TransmitRequest frameId (LongAddr dest) options d) =
                do put (0x00 :: Word8)
                   put frameId
                   put dest
                   put options
                   putByteString d

        put (TransmitRequest frameId (ShortAddr dest) options d) =
                do put (0x01 :: Word8)
                   put frameId
                   put dest
                   put options
                   putByteString d

        put x = fail $ printf "Putting %s not implemented" (show x)

data Frame = Frame APICmd deriving (Show, Eq)

instance Serialize Frame where
        put (Frame d) =
                do putWord8 0x7e
                   let payload = encode d
                       csum = 0xff - (BS.foldl1' (+) payload) .&. 0xff
                   putWord16be (fromIntegral $ BS.length payload)
                   putByteString payload
                   putWord8 (fromIntegral csum)
        get  =
                do delim <- get :: Get Word8
                   when (delim /= 0x7e) (fail "Incorrect start delimiter")
                   length <- get :: Get Word16
                   d <- getBytes (fromIntegral length)
                   csum <- get :: Get Word8
                   let csum' = csum + BS.foldl1' (+) d
                   when (csum' /= 0xff) (fail "Incorrect checksum")
                   case decode d of
                        Left a -> fail $ "Failed to parse command " ++ a
                        Right cmd -> return $ Frame cmd

