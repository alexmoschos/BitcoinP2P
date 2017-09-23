{-# LANGUAGE RecordWildCards #-}

module Main where

import           Protocol.Header
import           Protocol.Message

import           System.Environment
import           XmlParser

import qualified Data.ByteString.Char8      as BS
import           Network

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
    let (_,bin) = myEncode version
    BS.writeFile "out.txt" bin
--    let bin = encode' version

    putStrLn ""
    putStrLn "Give any input to continue:"
    _ <- getLine
    putStrLn $ "Connecting to " ++ xmlIp ++ ":" ++ show xmlPort

    h <- connectTo xmlIp $ PortNumber xmlPort
    BS.hPutStr h bin

    vmsg <- getMessage h
    print vmsg

    cmsg <- getMessage h
    print cmsg

    amsg <- getMessage h
    print amsg

    return ()
