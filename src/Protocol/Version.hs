{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Protocol.Version where

import           Lib

import           System.IO
import           System.Random
import           Network
import           Network.Socket
import           Crypto.Hash

import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get

import           Data.ByteString.Char8      as BS
import           Debug.Trace

data Version = Version MHeader MVersion

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

sizeOfVersionHeader :: Int
sizeOfVersionHeader = 24

data MHeader = MHeader {
    mMagic    :: Word32,
    mCommand  :: String,
    mPayload  :: Word32,
    mCheckSum :: BS.ByteString
} deriving (Show)

data MNetwork = MNetwork {
    mService :: Word64,
    mIP      :: SockAddr
} deriving (Show)

-- instance Show MVersion where


instance Binary Version where

  get = do
      head <- get
      body <- get
      return $ Version head body

  put (Version (MHeader mag ver _ _) msg) = do
      let payload= encode' msg
          chk = checksum payload
          len = traceShowId ((fromIntegral $ BS.length payload) :: Word32)
          --len = fromIntegral $ BS.length payload
          header = MHeader mag ver len chk
      putByteString $ encode' header `BS.append` payload

instance Binary MVersion where
    put MVersion {..} = do
        putWord32le mVersion
        putWord64le mServices
        putWord64le mTimestamp
--        putWord64le 1355854353 -- wiki default
        put mAddrRecv
        put mAddrFrom
        putWord64le mNonce
        let len = fromIntegral $ Prelude.length mUsrAgent
        putWord8 len
        putByteString $ BS.pack mUsrAgent
        putWord32le mStHeight
        putBoolVal (fromIntegral mVersion) mRelay

    get = do
        version <- getWord32le
        services <- getWord64le
        time <- getWord64le
        addrRec <- get
        addrFrom <- get
        nonce <- getWord64le
        len <- getWord8
        agent <- getByteString $ fromIntegral len
        height <- getWord32le
        rel <- getWord8
        let relbl = rel == 1
        return $ MVersion version services time addrRec addrFrom nonce (BS.unpack agent) height relbl

putBoolVal :: Int -> Bool -> Put
putBoolVal n bl = if n < 70001 then put "" else
    putWord8 $ if bl then 1 else 0


instance Binary MNetwork where
    put (MNetwork serv (SockAddrInet port addr)) = do
        putWord64le serv
        putWord32be 0x00000000
        putWord32be 0x00000000
        putWord32be 0x0000ffff
        putWord32host addr
        putWord16be $ fromIntegral port

    get = do
        serv <- getWord64le
        getWord32be
        getWord32be
        getWord32be
        addr <- getWord32be
        port <- getWord16be
        return $ MNetwork serv (SockAddrInet (fromIntegral port) addr)

instance Binary MHeader where
    put MHeader {..} = do
        putWord32le mMagic
        putByteString $ convert mCommand
        putWord32le mPayload
        putByteString mCheckSum
        -- putWord32le mCheckSum
    get = do
        magic <- getWord32le
        command <- getByteString 12
        payload <- getWord32le
        ckecksum <- getByteString 4
        return $ MHeader magic (show command) payload ckecksum

showMHeader :: MHeader -> String
showMHeader MHeader{..} = "Version Header:" ++
        "\n  Magic:    " ++ show mMagic ++
        "\n  Command:  " ++ show mCommand ++
        "\n  Payload:  " ++ show mPayload ++
        "\n  Checksum: " ++ show mCheckSum ++ "\n"

showMVersion :: MVersion -> String
showMVersion MVersion{..} = "Version:" ++
    "\n  Version   " ++ show mVersion   ++
    "\n  Services  " ++ show mServices  ++
    "\n  Timestamp " ++ show mTimestamp ++
    "\n  AddrRecv  " ++ show mAddrRecv  ++
    "\n  AddrFrom  " ++ show mAddrFrom  ++
    "\n  Nonce     " ++ show mNonce     ++
    "\n  UsrAgent  " ++ show mUsrAgent  ++
    "\n  StHeight  " ++ show mStHeight  ++
    "\n  Relay     " ++ show mRelay     ++
        if fromIntegral mVersion < 70001 then "    (not supported)" else "" ++
    "\n"

convert :: String -> BS.ByteString
convert str =
    BS.pack $ Prelude.take 12 $ str ++ Prelude.repeat '\NUL'
