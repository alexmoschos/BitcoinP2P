module Lib where

import           Crypto.Hash
import           Data.Binary
import qualified Data.ByteArray             as A
import qualified Data.ByteString            as B
import           Data.ByteString.Char8      as BS
import           Data.ByteString.Lazy.Char8 as BL
import           Network.Socket
import           Data.List.Split

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

toHostAddress :: String -> HostAddress
toHostAddress str =
    tupleToHostAddress (read (Prelude.head s), read(s !! 1), read(s !! 2), read(s !! 3))
    where
      s = splitOn "." str
