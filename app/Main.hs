{-# LANGUAGE RecordWildCards #-}
module Main where
import           Data.Binary
import           Data.Binary.Put
import           Data.Int
--import           Data.Word
import           Network.Socket
instance Binary MVersion where
    put MVersion {..} = do
        putWord32le mVersion
        {-
        put mServices
        put mTimestamp
        put mAddrRecv
        put mAddrFrom
        put mNonce
        put mUsrAgent
        put mRelay
        -}
    get = undefined
data MVersion = MVersion {
    mVersion   :: Word32,
    mServices  :: Word64,
    mTimestamp :: Int64,
    mAddrRecv  :: HostName,
    mAddrFrom  :: HostName,
    mNonce     :: Word64,
    mUsrAgent  :: String,
    mRelay     :: Bool
} deriving(Show)


main :: IO ()
main =
    let
        a = MVersion 31900 1 1 "1" "1" 1 "" False
    in
        print $ encode a
        --print $ decode $ encode a

