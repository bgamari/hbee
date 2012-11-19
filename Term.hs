import Data.Serialize
import qualified Data.ByteString as BS
import System.Hardware.XBee
import System.Hardware.XBee.Utils
import System.Hardware.Serialport hiding (send)
import Control.Monad
import Data.Monoid
import Data.List
import Control.Concurrent (forkIO)
import Options.Applicative

data Mode = LocalATMode
          | RemoteATMode Address
          | TerminalMode Address
          deriving (Show)

data Args = Args { port :: FilePath
                 , mode :: Mode
                 }
          deriving (Show)
          
readAddress :: String -> Maybe Address
readAddress s = do
    a <- read s
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

args = Args
    <$> strOption ( long "device"
                 <> short 'd'
                 <> metavar "FILENAME"
                 <> value "/dev/ttyUSB0"
                 <> help "Serial device with XBee attached"
                  )
    <*> subparser ( command "local-at"
                    (info localAtMode  (progDesc "Issue AT commands to local unit"))
                 <> command "remote-at"
                    (info remoteAtMode (progDesc "Issue AT commands to remote unit"))
                 <> command "terminal"
                    (info terminalMode (progDesc "Standard terminal passthrough mode"))
                 -- <> command "discover"
                 --    (info remoteAtMode (progDesc "Issue AT commands to local unit"))
                 -- <> command "measure"
                 --    (info measureMode (progDesc "Issue AT commands to local unit"))
                  )
   
argsInfo = info (helper <*> args)
    ( fullDesc
   <> progDesc "XBee Swiss Army knife"
   <> header "hbee-term - Swiss Army knife for Digi XBee radios"
    )

main = execParser argsInfo >>= \args->withXBee (port args) $ go (mode args)
     
readWorker ser = recvFrame ser >>= print

parseATCmd :: String -> (String, BS.ByteString)
parseATCmd s =
    let (cmd,args) = splitAt 2 $ maybe s id (s `stripPrefix` "AT")
    in (cmd, packToByteString args)

go :: Mode -> SerialPort -> IO ()
go LocalATMode ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= (\s->print (parseATCmd s) >> doCmd s)
    where doCmd s = let (cmd,args) = parseATCmd s
                    in sendFrame ser $ Frame
                       $ ATCommand { apiFrameId       = 1
                                   , apiATCommand     = cmd
                                   , apiParam         = args
                                   }

go (TerminalMode addr) ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= doCmd
    where doCmd s = sendFrame ser $ Frame
                    $ TransmitRequest { apiFrameId       = 1
                                      , apiDest          = addr
                                      , apiOptions       = 0
                                      , apiData          = packToByteString s
                                      }

go (RemoteATMode addr) ser = do
    forkIO (forever $ readWorker ser)
    forever $ getLine >>= doCmd
    where doCmd s = let (cmd,args) = parseATCmd s
                    in sendFrame ser $ Frame
                       $ RemoteATCommand { apiFrameId    = 1
                                         , apiDest       = addr
                                         , apiOptions    = 0
                                         , apiATCommand  = cmd
                                         , apiParam      = args
                                         }

