module Main where
import           Data.Int
import           Data.Word
import           Network.Socket
data Message = Message {
    mVersion   :: Int32,
    mServices  :: Word64,
    mTimestamp :: Int64,
    mAddrRecv  :: HostName,
    mAddrFrom  :: HostName,
    mNonce     :: Word64,
    mUsrAgent  :: String,
    mRelay     :: Bool
}

main :: IO ()
main = return ()
