module Protocol.Verack where

import           Protocol.Header
import           Data.Binary
import           Data.Binary.Put
import           Data.Binary.Get

data Verack = Verack deriving(Show)

instance Body Verack where
    myCommand = const "verack"
    myShow _ = "Verack:\n(empty)\n"

instance Binary Verack where
    put _ = return ()
    get = return Verack
