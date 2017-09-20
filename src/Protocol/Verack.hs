{-# LANGUAGE RecordWildCards   #-}

module Protocol.Verack where

import Protocol.Version

import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get

import           Data.ByteString.Char8      as BS

data Verack = Verack {
      vrMagic    :: Word32
    , vrCommand  :: String
    , vrPayloadLen :: Word32
    , vrCheckSum :: BS.ByteString
} deriving (Show)

sizeOfVerackHeader :: Int
sizeOfVerackHeader = 24

instance Binary Verack where
    get = do
        magic <- getWord32le
        command <- getByteString 12
        payload <- getWord32le
        ckecksum <- getByteString 4
        return $ Verack magic (show command) payload ckecksum

    put Verack{..} = do
      putWord32le vrMagic
      putByteString $ convert vrCommand
      putWord32le vrPayloadLen
      putByteString vrCheckSum

showVerack :: Verack -> String
showVerack Verack{..} = "Verack:" ++
        "\n  Magic:    " ++ show vrMagic ++
        "\n  Command:  " ++ show vrCommand ++
        "\n  Payload:  " ++ show vrPayloadLen ++
        "\n  Checksum: " ++ show vrCheckSum ++ "\n"
