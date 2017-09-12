{-# LANGUAGE RecordWildCards #-}
module Main where
import           Data.Binary
import           Data.Binary.Put
import           Data.Int
--import           Data.Word
import           Data.Time.Clock.POSIX
import           Network.Socket
import           System.Random
instance Binary MVersion where
    put MVersion {..} = do
        putWord32le mVersion
        putWord64le mServices
        putWord64le mTimestamp
        put mAddrRecv
        put mAddrFrom
        putWord64le mNonce
        put mUsrAgent
        putWord32le mStHeight
        putBoolVal mRelay
    get = undefined

putBoolVal :: Bool -> Put
putBoolVal True  = putWord8 1
putBoolVal False = putWord8 0

data MVersion = MVersion {
    mVersion   :: Word32,
    mServices  :: Word64,
    mTimestamp :: Word64,
    mAddrRecv  :: MNetwork,
    --network adress struct needed
    mAddrFrom  :: MNetwork,
    mNonce     :: Word64,
    mUsrAgent  :: String,
    mStHeight  :: Word32,
    mRelay     :: Bool
} deriving(Show)

instance Binary MNetwork where
    put MNetwork {..} = do
        putWord64le mService
        put mIp
        putWord16le mPort
    get = undefined

data MHeader = MHeader {
    mMagic :: Word32,
    mCommand :: [String],
    mPayload :: Word32,
    mCheckSum :: Word32
} deriving (Show)

instance Binary MHeader where
   put MHeader {..} = do
       putWord32le mMagic
       put mCommand
       putWord32le mPayload
       putWord32le mCheckSum

data MNetwork = MNetwork {
    --mTime    :: Word64,
    mService :: Word64,
    mIp      :: String,
    mPort    :: Word16
} deriving (Show)

main :: IO ()
main = do
    nonce <- randomIO :: IO Word64
    time <- getPOSIXTime
    let t = round time
        services = 1
        host = MNetwork services "127.0.0.1" 44
        host2 = MNetwork services "127.0.0.1" 44
        -- Theloume to user agent na einai 0x00 1byte
        a = MVersion 31900 services t host host2 1 "" 0 False
    print a
    encodeFile "a.txt" a

