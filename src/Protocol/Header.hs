{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveAnyClass #-}

module Protocol.Header where

import           Lib

import           Network.Socket
import qualified Data.Char as C

import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get

import           Data.ByteString.Char8      as BS
import           Debug.Trace

sizeOfHeader :: Int
sizeOfHeader = 24

magic :: String
magic = "0xd9b4bef9"

data Header = Header {
    mMagic    :: Word32,
    mCommand  :: String,
    mPayload  :: Word32,
    mCheckSum :: BS.ByteString
} deriving (Show)

instance Binary Header where
    put Header{..} = do
        putWord32le mMagic
        putByteString $ convert mCommand
        putWord32le mPayload
        putByteString mCheckSum
        -- putWord32le mCheckSum

    get = do
        mgc <- getWord32le
        command <- getByteString 12
        let comm = Prelude.filter ( /= '\x00') $ show command
        payload <- getWord32le
        ckecksum <- getByteString 4
        return $ Header mgc comm payload ckecksum

showMHeader :: Header -> String
showMHeader Header{..} = capitalized mCommand ++ " Header:" ++
        "\n  Magic:    " ++ show mMagic ++
        "\n  Command:  " ++ mCommand ++
        "\n  Payload:  " ++ show mPayload ++
        "\n  Checksum: " ++ show mCheckSum ++ "\n"

class Binary b => Body b where
    myCommand :: b -> String
    myShow :: b -> String
    createHeader :: Binary b => b -> (Header, ByteString)
    createHeader = createHeaderDefault
    myEncode :: Binary b => b -> (Header,ByteString)
    myEncode = myEncodeDefault

    createHeaderDefault :: Body b => b -> (Header, ByteString)
    createHeaderDefault b =
      let
        payload = encode' b
        chk = checksum payload
        len = traceShowId ((fromIntegral $ BS.length payload) :: Word32)
        --len = fromIntegral $ BS.length payload
        header = Header (read magic) (myCommand b) len chk
      in
        (header,payload)

    myEncodeDefault :: Body b => b -> (Header, ByteString)
    myEncodeDefault b =
      let
          (header, payload) = createHeader b
      in
          (header,encode' header `BS.append` payload)

data MNetwork = MNetwork {
    mService :: Word64,
    mIP      :: SockAddr
} deriving (Show)

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

convert :: String -> BS.ByteString
convert str =
    BS.pack $ Prelude.take 12 $ str ++ Prelude.repeat '\NUL'

capitalized :: String -> String
capitalized (head:tail) = C.toUpper head : Prelude.map C.toLower tail
capitalized [] = []
