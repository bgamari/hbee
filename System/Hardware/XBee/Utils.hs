module System.Hardware.XBee.Utils ( unpackToString
                                  , packToByteString
                                  , getRemainingBytes
                                  ) where

import qualified Data.ByteString as BS
import Data.Serialize

unpackToString :: BS.ByteString -> String
unpackToString = map (toEnum . fromIntegral) . BS.unpack

packToByteString :: String -> BS.ByteString
packToByteString = BS.pack . map (fromIntegral . fromEnum)

getRemainingBytes = remaining >>= getBytes

