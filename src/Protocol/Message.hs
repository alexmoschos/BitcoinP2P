{-# LANGUAGE RecordWildCards   #-}

module Protocol.Message where

import           Protocol.Header
import           Protocol.Version
import           Protocol.Verack

import           Network
import           Network.Socket
import           System.IO
import qualified Data.List as L

import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL

data Package = Package Header Message
    deriving(Show)

data Message =
          MVersion Version
        | MVerack Verack
    deriving(Show)

instance Binary Message where
    put (MVersion m) = put m
    put (MVerack  m) = put m

    get = undefined

instance Binary Package where
    put (Package h m) = do
        put h
        put m

    get = undefined

getMessage :: Handle -> IO Package
getMessage h = do
    hdBin <- BL.hGet h sizeOfHeader
    let
        hd@Header{..} = decode hdBin

    let len = mPayload

    bodyBin <- BL.hGet h (fromIntegral len)
    print $ BL.length bodyBin
    let
        msg
            | L.isInfixOf "version" mCommand = MVersion $ decode bodyBin
            | L.isInfixOf "verack"  mCommand = MVerack  $ decode bodyBin
    return $ Package hd msg
