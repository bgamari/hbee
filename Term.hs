import Data.Serialize
import qualified Data.ByteString as BS
import System.Hardware.XBee
import System.Hardware.XBee.Utils
import System.Hardware.XBee.Discover
import System.Hardware.Serialport hiding (send)
import Control.Monad
import Data.Monoid
import Data.Char
import Data.List
import Data.Bits
import Control.Concurrent (forkIO)
import Options.Applicative
import Text.Printf       

data Mode = LocalATMode
          | RemoteATMode Address
          | TerminalMode Address
          | DiscoverMode
          deriving (Show)

data Args = Args { port :: FilePath
                 , mode :: Mode
                 }
          deriving (Show)
          
readAddress :: String -> Maybe Address
readAddress s = do
    a <- auto s
    return $ if a < 65536 then ShortAddr $ fromInteger a
                          else LongAddr $ fromInteger a
    
terminalMode :: Parser Mode
terminalMode = TerminalMode
    <$> nullOption ( long "address"
                  <> short 'a'
                  <> reader readAddress
                  <> help "Address of remote unit"
                   )

remoteAtMode :: Parser Mode
remoteAtMode = RemoteATMode
    <$> nullOption ( long "address"
                  <> short 'a'
                  <> reader readAddress
                  <> help "Address of remote unit"
                   )

localAtMode :: Parser Mode
localAtMode = pure LocalATMode

discoverMode :: Parser Mode
discoverMode = pure DiscoverMode

args = Args
    <$> strOption ( long "device"
                 <> short 'd'
                 <> metavar "FILENAME"
                 <> value "/dev/ttyUSB0"
                 <> help "Serial device with XBee attached"
                  )
    <*> subparser ( command "local-at"
                    (info localAtMode  (progDesc "Issue AT commands to local node"))
                 <> command "remote-at"
                    (info remoteAtMode (progDesc "Issue AT commands to remote node"))
                 <> command "terminal"
                    (info terminalMode (progDesc "Standard terminal passthrough mode"))
                 <> command "discover"
                    (info discoverMode (progDesc "Discover neighboring nodes"))
                 -- <> command "measure"
                 --    (info measureMode (progDesc "Issue AT commands to local unit"))
                  )
   
argsInfo = info (helper <*> args)
    ( fullDesc
   <> progDesc "XBee Swiss Army knife"
   <> header "hbee-term - Swiss Army knife for Digi XBee radios"
    )

main = execParser argsInfo >>= \args->withXBee (port args) $ go (mode args)
     
escapeBS :: BS.ByteString -> String
escapeBS = concatMap (escape . chr . fromIntegral) . BS.unpack
    where escape c | isPrint c = [c]
                   | otherwise = "\\"++printf "%02x" c
     
readWorker ser = do
    frame <- recvFrame ser
    putStr $ either (\s->"--- error: "<>s<>"\n") (\(Frame f)->showCmd f) frame
           
showAddr :: Address -> String
showAddr (LongAddr a) = printf "0x%08x" a
showAddr (ShortAddr a) = printf "0x%02x" a

showCmd :: APICmd -> String
showCmd (ModemStatus {apiStatus=status}) =
    "<<< status = "<>show status<>"\n"
showCmd resp@(ATResponse {}) =
    "=<= "<>status<>" AT"<>apiATCommand resp<>" = "<>escapeBS (apiCmdData resp)<>"\n"
    where status = case apiStatus resp of
                          0x0 -> "success"
                          0x1 -> "error"
                          0x2 -> "invalid command"
                          0x3 -> "invalid parameter"
                          a   -> "unknown status: "++show a

showCmd resp@(RemoteATResponse {}) =
    "=<= from "<>showAddr (apiSource resp)<>": "
    <>status<>" AT"<>apiATCommand resp<>" = "<>escapeBS (apiCmdData resp)<>"\n"
    where status = case apiStatus resp of
                          0x0 -> "success"
                          0x1 -> "error"
                          0x2 -> "invalid command"
                          0x3 -> "invalid parameter"
                          0x4 -> "no response"
                          a   -> "unknown status: "++show a
    
showCmd resp@(TransmitStatus {}) =
    "=<= TX status "<>status<>"\n"
    where status = case apiTxStatus resp of
                          TxSuccess    -> "success"
                          TxNoAck      -> "no ack"
                          TxCCAFailure -> "cca failure"
                          TxPurged     -> "purged"

showCmd resp@(Receive {}) =
    "<<< from "<>showAddr (apiSource resp)<>" (RSSI="<>show (apiRSSI resp)<>"): "
    <>escapeBS (apiData resp)<>"\n"

showCmd resp@(IOData {}) =
    "<<< from "<>showAddr (apiSource resp)<>" (RSSI="<>show (apiRSSI resp)<>"): "
    <>intercalate " " (map showDChannel $ apiDChannels resp)
    <>"  "
    <>intercalate " " (map showAChannel $ apiAChannels resp)
    <>"\n"
    where showDChannel (ch,state) = "D"<>show ch<>"="<>if state then "1" else "0"
          showAChannel (ch,value) = "A"<>show ch<>"="<>printf "%02x" value

showCmd resp@(ATCommand {}) =
    "=>= AT"<>apiATCommand resp<>" = "<>escapeBS (apiParam resp)<>"\n"

showCmd resp@(ATQueueCommand {}) =
    "=>= queue AT"<>apiATCommand resp<>" = "<>escapeBS (apiParam resp)<>"\n"

showCmd resp@(RemoteATCommand {}) =
    "=>= to "<>showAddr (apiDest resp)<>": "
    <>"AT"<>apiATCommand resp<>" = "<>escapeBS (apiParam resp)<>"\n"

showCmd resp@(TransmitRequest {}) =
    ">>> to "<>showAddr (apiDest resp)<>" ("<>intercalate " " options<>"): "
    <>escapeBS (apiData resp)<>"\n"
    where options = let o = apiOptions resp
                    in concat $ [ if o `testBit` 0 then ["no-ack"] else []
                                , if o `testBit` 2 then ["broadcast"] else []
                                ]

parseATCmd :: String -> (String, String)
parseATCmd s =
    let (cmd,args) = splitAt 2 $ maybe s id (s `stripPrefix` "AT")
    in (map toUpper cmd, args)

go :: Mode -> SerialPort -> IO ()
go LocalATMode ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= doCmd
    where doCmd s = let (atCmd,args) = parseATCmd s
                        cmd = ATCommand { apiFrameId       = 1
                                        , apiATCommand     = atCmd
                                        , apiParam         = packToByteString args
                                        }
                    in do sendFrame ser $ Frame cmd
                          putStr $ showCmd cmd

go (TerminalMode addr) ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= doCmd
    where doCmd s = let cmd = TransmitRequest { apiFrameId       = 1
                                              , apiDest          = addr
                                              , apiOptions       = 0
                                              , apiData          = packToByteString s
                                              }
                    in do sendFrame ser $ Frame cmd
                          putStr $ showCmd cmd

go (RemoteATMode addr) ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= doCmd
    where doCmd s = let (atCmd,args) = parseATCmd s
                        cmd = RemoteATCommand { apiFrameId    = 1
                                              , apiDest       = addr
                                              , apiOptions    = 0
                                              , apiATCommand  = atCmd
                                              , apiParam      = case reads args of
                                                                   (a,_):_ -> runPut $ putWord32be a
                                                                   _       -> packToByteString args
                                              }
                    in do sendFrame ser $ Frame cmd
                          putStr $ showCmd cmd

go (DiscoverMode) ser = discoverNodes ser >>= mapM_ print
