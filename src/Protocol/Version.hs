{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Protocol.Version where

import           Protocol.Header

import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get

import           Data.ByteString.Char8      as BS

data Version = Version {
    mVersion   :: Word32,
    mServices  :: Word64,
    mTimestamp :: Word64,
    mAddrRecv  :: MNetwork,
    -- network adress struct needed
    mAddrFrom  :: MNetwork,
    mNonce     :: Word64,
    -- mUsrAgent  :: VarString,
    mUsrAgent  :: String,
    mStHeight  :: Word32,
    mRelay     :: Bool
} deriving(Show)

instance Binary Version where
    put Version {..} = do
        putWord32le mVersion
        putWord64le mServices
        putWord64le mTimestamp
--        putWord64le 1355854353 -- wiki default
        put         mAddrRecv
        put         mAddrFrom
        putWord64le mNonce
        let len = fromIntegral $ Prelude.length mUsrAgent
        putWord8    len
        putByteString $ BS.pack mUsrAgent
        putWord32le mStHeight
        putBoolVal mVersion mRelay

    get = do
        version  <- getWord32le
        services <- getWord64le
        time     <- getWord64le
        addrRec  <- get
        addrFrom <- get
        nonce    <- getWord64le
        len      <- getWord8
        agent    <- getByteString $ fromIntegral len
        height   <- getWord32le
        rel      <- if hasRelay version then getWord8 else return 0
        let relbl = rel == 1
        return $ Version version services time addrRec addrFrom nonce (BS.unpack agent) height relbl

instance Body Version where
    myCommand = const "version"
    myShow = showMVersion

showMVersion :: Version -> String
showMVersion Version{..} = "Version:" ++
    "\n  Version   " ++ show mVersion   ++
    "\n  Services  " ++ show mServices  ++
    "\n  Timestamp " ++ show mTimestamp ++
    "\n  AddrRecv  " ++ show mAddrRecv  ++
    "\n  AddrFrom  " ++ show mAddrFrom  ++
    "\n  Nonce     " ++ show mNonce     ++
    "\n  UsrAgent  " ++ show mUsrAgent  ++
    "\n  StHeight  " ++ show mStHeight  ++
    "\n  Relay     " ++ show mRelay     ++
        if hasRelay mVersion then "    (not supported)" else "" ++
    "\n"

putBoolVal :: Word32 -> Bool -> Put
putBoolVal n bl = if hasRelay n then put "" else
    putWord8 $ if bl then 1 else 0

hasRelay :: Word32 -> Bool
hasRelay n = fromIntegral n < (70001 :: Int)
