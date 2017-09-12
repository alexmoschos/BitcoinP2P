{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where
import           Data.Binary
import           Data.Binary.Put
import           Data.Int
--import           Data.Word
import           Data.Time.Clock.POSIX
import           Network.Socket
import           System.Random
-- import           Data.ByteString
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 as BL
import           Debug.Trace

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
    mMagic    :: Word32,
    mCommand  :: String,
    mPayload  :: Word32,
    mCheckSum :: Word32
} deriving (Show)

instance Binary MHeader where

    put MHeader {..} = do
        putWord32be mMagic
        --putWord32le 42
        putByteString $ convert mCommand
        putWord32le mPayload
        putWord32le mCheckSum
    get = undefined

data MNetwork = MNetwork {
    --mTime    :: Word64,
    mService :: Word64,
    mIp      :: String,
    mPort    :: Word16
} deriving (Show)

convert :: String -> BS.ByteString
convert str =
    BS.pack $ Prelude.take 12 $ "version" ++ Prelude.repeat '\NUL'
  -- let
  --       bs = pack str
  --       go 0 bs = bs
  --       go n bs = go (n-1) $ snock
  --   in
  --       go n bs
  --

main :: IO ()
main = do
    nonce <- randomIO :: IO Word64
    time <- getPOSIXTime
    let t = round time
        services = 1
        host = MNetwork services "127.0.0.1" 44
        host2 = MNetwork services "127.0.0.1" 44
        -- Theloume to user agent na einai 0x00 1byte
        a = MVersion 31900 services t host host2 nonce "" 0 False
        b = MHeader 0 "version" 85 0
        rrr = A b a
    -- print a
    encodeFile "a.txt" rrr

data A = A MHeader MVersion
instance Binary A where

    get = undefined

    put (A _ msg) = do
        let payload= encode' msg
            chk = 0
            len = traceShowId ((fromIntegral $ BS.length payload) :: Word32)
            --len = fromIntegral $ BS.length payload
            header = MHeader 0 "version" len chk
            --header = MessageHeader networkMagic cmd len chk
        putByteString $ (encode' header) `BS.append` payload

encode' :: Binary a => a -> BS.ByteString
encode' = toStrictBS . encode

-- | Transforms a lazy bytestring into a strict bytestring
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks
