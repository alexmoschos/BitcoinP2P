{-# LANGUAGE RecordWildCards #-}
module Main where
import           Data.Binary
import           Data.Binary.Put
import           Data.Int
--import           Data.Word
import           Data.Time.Clock.POSIX
import           Network.Socket
instance Binary MVersion where
    put MVersion {..} = do
        putWord32le mVersion
        putWord64le mServices
        putWord64le mTimestamp
        put mAddrRecv
        put mAddrFrom
        putWord64le mNonce
        put mUsrAgent
        putBoolVal mRelay
    get = undefined

putBoolVal :: Bool -> Put
putBoolVal True  = putWord8 1
putBoolVal False = putWord8 0

data MVersion = MVersion {
    mVersion   :: Word32,
    mServices  :: Word64,
    mTimestamp :: Word64,
    mAddrRecv  :: HostName,
    --network adress struct needed
    mAddrFrom  :: HostName,
    mNonce     :: Word64,
    mUsrAgent  :: String,
    mRelay     :: Bool
} deriving(Show)


main :: IO ()
main = do
    time <- getPOSIXTime
    let t = round time
        a = MVersion 31900 1 t "127.0.0.1" "127.0.0.1" 1 "" False
    print a
    encodeFile "a.txt" a

