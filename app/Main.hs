{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Protocol.Version
import           Protocol.Verack

import           System.Environment
import           Lib
import           XmlParser
import           Data.Binary

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import           Network
import           Network.Socket

main :: IO ()
main = do
    [path] <- getArgs
    main2 path

main2 :: FilePath -> IO ()
main2 path = do
    print path
    input@XMLreader{..} <- xmlParser path

    putStrLn "Input from XML:"
    print input
    putStrLn ""
    putStrLn "Encoding..."

    version <- parseVersion input
    encodeFile "out.txt" version
    let bin = encode' version

    putStrLn ""
    putStrLn "Give any input to continue:"
    getLine
    putStrLn $ "Connecting to " ++ xmlIp ++ ":" ++ show xmlPort

    h <- connectTo xmlIp $ PortNumber xmlPort
    BS.putStrLn bin
    BS.hPutStr h bin

    hdBin <- BL.hGet h sizeOfVersionHeader
    print $ BL.length hdBin
    let Right (_, _, head) = decodeOrFail hdBin
    let dummy = head :: MHeader
    putStrLn $ showMHeader head

    let len = mPayload head

    bodyBin <- BL.hGet h (fromIntegral len)
    print $ BL.length bodyBin
    let Right (_, _, body) = decodeOrFail bodyBin
    let dummy' = body :: MVersion
    putStrLn $ showMVersion body

    verackBin <- BL.hGet h (fromIntegral sizeOfVerackHeader)
    print $ BL.length verackBin
    let Right (_, _, verack) = decodeOrFail verackBin
    let dummy'' = verack :: Verack
    putStrLn $ showVerack verack

    return ()
