import System.Hardware.XBee
import System.Hardware.XBee.Discover

main = withXBee "/dev/ttyUSB0" test
test ser = discoverNodes ser >>= mapM_ print

