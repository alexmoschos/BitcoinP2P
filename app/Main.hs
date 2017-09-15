{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import Data.List as List
import           System.Environment
import           Crypto.Hash
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get
import qualified Data.ByteArray             as A
import qualified Data.ByteString            as B
-- import           Data.Int
-- import           Data.Word
import           Data.Time.Clock.POSIX
import           Data.List.Split
import           Network.Socket
import           System.Random
-- import           Data.ByteString
import           Data.ByteString.Lazy as L
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 as BL
import           Debug.Trace
import           Network
import           System.IO
import           Text.XML.Light

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
--        putBoolVal mRelay

    get =
        do
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

putBoolVal :: Bool -> Put
putBoolVal True  = putWord8 1
putBoolVal False = putWord8 0

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

instance Binary MNetwork where
    put (MNetwork serv (SockAddrInet port addr)) = do
            putWord64le serv
            putWord32be 0x00000000
            putWord32be 0x00000000
            putWord32be 0x0000ffff
            putWord32host addr
            putWord16be $ fromIntegral port

    get =
          do
        serv <- getWord64le
        getWord32be
        getWord32be
        getWord32be
        addr <- getWord32be
        port <- getWord16be
        return $ MNetwork serv (SockAddrInet (fromIntegral port) addr)

data MHeader = MHeader {
    mMagic    :: Word32,
    mCommand  :: String,
    mPayload  :: Word32,
    mCheckSum :: BS.ByteString
    -- mCheckSum :: Word32
} deriving (Show)

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

data MNetwork = MNetwork {
    mService :: Word64,
    mIP      :: SockAddr
} deriving (Show)

convert :: String -> BS.ByteString
convert str =
    BS.pack $ Prelude.take 12 $ str ++ Prelude.repeat '\NUL'

data XMLreader =
        XMLreader {
           xmlMagic :: Word32
        ,  xmlCommand :: String
        ,  xmlSatoshi :: String
        ,  xmlVersion :: Word32
        ,  xmlTime :: Maybe Word64
        ,  xmlNonce :: Maybe Word64
        ,  xmlMyIp :: String
        ,  xmlMyPort :: PortNumber
        ,  xmlIp :: String
        ,  xmlPort :: PortNumber
        ,  xmlServices :: Word64
        ,  xmlBlockId :: Word32
        ,  xmlRelay :: Bool
        } deriving (Show)

xmlParser :: String -> IO XMLreader
xmlParser path = do
  str <- System.IO.readFile path
  let
      contents = parseXML str
      quotes   = findEl "Version"
      Just strMagic = findAttr' "Magic"
      magic = read strMagic
      Just command = findAttr' "Command"
      Just satoshi = findAttr' "Satoshi"
      Just strVersion = findAttr' "Protocol"
      version = read strVersion

      myIp = findContent "MyIp"
      myPort = read $ findContent "MyPort"

      ip = findContent "Ip"
      port = read $ findContent "Port"
      services = read $ findContent "Services"
      blockId = read $ findContent "BlockId"
      relay = read $ findContent "Relay"

      time = case findEl "Time" of
          [] -> Nothing
          _  -> Just $ read $ findContent "Time"
      nonce = case findEl "Nonce" of
          [] -> Nothing
          _  -> Just $ read $ findContent "Nonce"
      findContent str = strContent $ Prelude.head $ findEl str
      findEl str = Prelude.concatMap (findElements $ simpleName str) (onlyElems contents)
      findAttr' str = Prelude.head $ Prelude.map (findAttr $ simpleName str) quotes
      simpleName s = QName s Nothing Nothing
  return $ XMLreader magic command satoshi version time nonce myIp myPort ip port services blockId relay
--parsexml txt = parse defaultParseOptions txt :: (LNode String String, Maybe XMLParseError)

main :: IO ()
main = do
    [path] <- getArgs
    print path
    input <- xmlParser path
    System.IO.putStrLn "Input from XML:"
    print input
    System.IO.putStrLn ""
    main2 input

main2 :: XMLreader -> IO ()
main2 XMLreader{..} = do
--    nonce <- randomIO :: IO Word64
    nonce <- case xmlNonce of
      Nothing -> randomIO :: IO Word64
      Just n -> return n
    time <- case xmlTime of
      Nothing -> round <$> getPOSIXTime
      Just t -> return t
    --let t = round time
    let
        host = MNetwork xmlServices $ SockAddrInet xmlPort $ toHostAddress xmlIp -- tupleToHostAddress (0,0,0,0) --(88,99,175,119)
        myhost = MNetwork xmlServices $ SockAddrInet xmlMyPort $ toHostAddress xmlMyIp --(0,0,0,0)
--        nonceWikiRev = 0x3B2EB35D8CE61765--15720238671696065164
--        nonceWiki = read "0x6517E68C5DB32E3B"
        head = MHeader xmlMagic xmlCommand 85 (BS.pack "0")
        body = MVersion xmlVersion xmlServices time host myhost nonce xmlSatoshi xmlBlockId xmlRelay
        bin = Version head body
        listenChars h = do
            c <- hGetChar h
            putChar c
            listenChars h
    System.IO.putStrLn "Encoding..."
    encodeFile "out.txt" bin
    System.IO.putStrLn ""

    System.IO.putStrLn "Give any input to continue:"
    System.IO.getLine
    System.IO.putStrLn $ "Connecting to " ++ xmlIp ++ ":" ++ show xmlPort
    h <- connectTo xmlIp $ PortNumber xmlPort
    BS.putStrLn $ encode' bin
    BS.hPutStr h $ encode' bin
--    msg <- System.IO.hGetLine h
--    print msg
    hd <- BL.hGet h 24
    print $ BL.length hd
    let Right (bs, bo, a) = decodeOrFail hd
    let ls = go a
    print a

    let MHeader q w payload r = a

    body <- BL.hGet h (fromIntegral payload)
    print $ BL.length body
    let Right (bs', bo', a') = decodeOrFail body
    let ls' = go' a'
    print a'
    return ()

go :: MHeader -> MHeader
go ls = ls

go' :: MVersion -> MVersion
go' ls = ls

data Version = Version MHeader MVersion
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

toHostAddress :: String -> HostAddress
toHostAddress str =
    tupleToHostAddress (read (List.head s), read(s !! 1), read(s !! 2), read(s !! 3))
    where
      s = splitOn "." str

encode' :: Binary a => a -> BS.ByteString
encode' = toStrictBS . encode

-- | Transforms a lazy bytestring into a strict bytestring
toStrictBS :: BL.ByteString -> BS.ByteString
toStrictBS = BS.concat . BL.toChunks

-- |Computes the SHA256 hash of the given @'ByteString'@.
sha256 :: BS.ByteString -> BS.ByteString
sha256 bs = let digest = hash bs :: Digest SHA256 in B.pack $ A.unpack digest

-- | Computes first 4 bytes of sha256(sha256(payload)).
checksum :: BS.ByteString -> BS.ByteString
checksum bs = BS.take 4 (sha256.sha256 $ bs)
