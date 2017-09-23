{-# LANGUAGE RecordWildCards   #-}

module Protocol.Message where

import           Protocol.Header
import           Protocol.Version
import           Protocol.Verack
import           Protocol.Addr

import           System.IO
import qualified Data.List as L

import           Data.Binary
import qualified Data.ByteString.Lazy.Char8 as BL

data Package = Package Header Message
    deriving(Show)

data Message =
          NoMessage
        | MVersion Version
        | MVerack  Verack
        | MAddr    Addr
    deriving(Show)

instance Binary Message where
    put (MVersion m) = put m
    put (MVerack  m) = put m
    put (MAddr  m) = put m

    get = undefined

instance Binary Package where
    put (Package h m) = do
        put h
        put m

    get = undefined

getMessage :: Handle -> IO Package
getMessage h = do
    hdBin <- BL.hGet h sizeHeader
    let
        hd@Header{..} = decode hdBin

    let len = mPayload

    bodyBin <- BL.hGet h (fromIntegral len)
    print $ BL.length bodyBin
    msg <-
        if      L.isInfixOf "version" mCommand
        then    return $ MVersion $ decode bodyBin
        else if L.isInfixOf "verack"  mCommand
        then    return $ MVerack  $ decode bodyBin
        else if L.isInfixOf "addr"  mCommand
        then    return $ MAddr $ decode bodyBin
        else    return NoMessage
    return $ Package hd msg
