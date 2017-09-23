{-# LANGUAGE RecordWildCards   #-}

module XmlParser where

import           Protocol.Header
import           Protocol.Version
import           Lib

import           Data.Binary
import           Data.Time.Clock.POSIX
import           System.Random
import           Network
import           Network.Socket
import           System.IO
import           Text.XML.Light
import           Data.ByteString.Char8      as BS

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


parseVersion :: XMLreader -> IO Version
parseVersion XMLreader{..} = do
  --        nonceWikiRev = 0x3B2EB35D8CE61765--15720238671696065164
  --        nonceWiki = read "0x6517E68C5DB32E3B"
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
      head = Header xmlMagic xmlCommand 85 (BS.pack "0")
      body = Version xmlVersion xmlServices time host myhost nonce xmlSatoshi xmlBlockId xmlRelay
--      version = Version head body
  return body
