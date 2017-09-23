{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Protocol.Addr where

import           Protocol.Header

import           Data.Binary

data Addr = Addr {
      aPayload :: Word8
    , aPeers    :: [MNetwork]
} deriving (Show)

instance Binary Addr where

    put Addr {..} = do
        put aPayload
        putList aPeers

    get = do
        num <- getWord8
        peers <- get
        return $ Addr (fromIntegral num) peers
{-}
getAddr :: Handle -> IO Addr
getAddr h = do
    payLoadBin <- BL.hGet h 1
    let num = decode payLoadBin
    addrBin <- BL.hGet h (num*sizeNetwork)
    let peers = decode addrBin
    return $ Addr (fromIntegral num) peers
-}

instance Body Addr where
    myCommand = const "addr"
    myShow = show
