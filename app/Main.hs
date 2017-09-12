{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Crypto.Hash
import           Data.Binary
import           Data.Binary.Put
import qualified Data.ByteArray             as A
import qualified Data.ByteString            as B
-- import           Data.Int
-- import           Data.Word
import           Data.Time.Clock.POSIX
import           Network.Socket
import           System.Random
-- import           Data.ByteString
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 as BL
import           Debug.Trace
import           Network
import           System.IO

-- newtype VarInt = VarInt { getVarInt :: Word64 }
--     deriving (Eq, Show, Read)

newtype VarInt = VarInt Word64



instance Binary MVersion where
    put MVersion {..} = do
        putWord32le mVersion
        putWord64le mServices
        putWord64le mTimestamp
        put mAddrRecv
        put mAddrFrom
        putWord64le mNonce
        let len = fromIntegral $ Prelude.length mUsrAgent
        putWord8 len
        putByteString $ BS.pack mUsrAgent
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
    -- mUsrAgent  :: VarString,
    mUsrAgent  :: String,
    mStHeight  :: Word32,
    mRelay     :: Bool
} deriving(Show)

instance Binary MNetwork where
    put (MNetwork serv (SockAddrInet port addr)) = do
            putWord64le serv
            putWord32be 0x00000000
            putWord32be 0x00000000
            putWord32be 0x0000ffff
            putWord32host addr
            putWord16be $ fromIntegral port

    -- put MNetwork {..} = do
    --     putWord64le mService
    --     put mIp
    --     putWord16le mPort
    get = undefined

data MHeader = MHeader {
    mMagic    :: Word32,
    mCommand  :: String,
    mPayload  :: Word32,
    mCheckSum :: BS.ByteString
    -- mCheckSum :: Word32
} deriving (Show)

instance Binary MHeader where

    put MHeader {..} = do
        putWord32le mMagic
        putByteString $ convert mCommand
        putWord32le mPayload
        putByteString mCheckSum
        -- putWord32le mCheckSum
    get = undefined

data MNetwork = MNetwork {
    --mTime    :: Word64,
    mService :: Word64,
    mIP      :: SockAddr
    -- mIp      :: String,
    -- mPort    :: Word16
} deriving (Show)

convert :: String -> BS.ByteString
convert str =
    BS.pack $ Prelude.take 12 $ str ++ Prelude.repeat '\NUL'

main :: IO ()
main = do
    nonce <- randomIO :: IO Word64
    time <- getPOSIXTime
    let t = round time
        services = 1
        host = MNetwork services $ SockAddrInet (8333::PortNumber) $ tupleToHostAddress (139,99,131,171)
        myhost = MNetwork services $ SockAddrInet (44::PortNumber) $ tupleToHostAddress (87,203,108,87)
        -- host2 = MNetwork services "127.0.0.1" 44
        -- Theloume to user agent na einai 0x00 1byte
        -- a = MVersion 60002 services t host host nonce "" 0 False
        a = MVersion 60002 services t host myhost nonce "/Satoshi:0.7.2/" 0 False
        -- a = MVersion 31900 services 0x0000000000000000 host host2 0x0000000000000000 "" 0 False
        b = MHeader 0 "version" 85 (BS.pack "0")
        rrr = A b a
    -- print a
    print "Hello"
    h <- connectTo "ns537674.ip-139-99-131.net" $ PortNumber 8333
    print "Hello"

    hPrint h $ encode' rrr
    print "Hello"

    encodeFile "a.txt" rrr

data A = A MHeader MVersion
instance Binary A where

    get = undefined

    put (A _ msg) = do
        let payload= encode' msg
            -- chk = 0
            -- chk' = checksum $ BS.pack "qwertyuiopasdfghjklzxcvbnm"
            chk = checksum payload
            len = traceShowId ((fromIntegral $ BS.length payload) :: Word32)
            --len = fromIntegral $ BS.length payload
            header = MHeader 0xd9b4bef9 "version" len chk
            --header = MessageHeader networkMagic cmd len chk
        putByteString $ encode' header `BS.append` payload
        -- putByteString $ encode' header


encode' :: Binary a => a -> BS.ByteString
encode' = toStrictBS . encode

-- | Transforms a lazy bytestring into a strict bytestring
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

-- |Computes the SHA256 hash of the given @'ByteString'@.
sha256 :: BS.ByteString -> BS.ByteString
sha256 bs = let digest = hash bs :: Digest SHA256 in B.pack $ A.unpack digest

-- | Computes first 4 bytes of sha256(sha256(payload)).
checksum :: BS.ByteString -> BS.ByteString
checksum bs = BS.take 4 (sha256.sha256 $ bs)
