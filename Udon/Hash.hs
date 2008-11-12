module Udon.Hash where

import qualified Data.ByteString.Lazy as Str
import qualified Data.Digest.SHA256 as SHA
import Data.Binary
import Control.Applicative

newtype Hash = Hash Str.ByteString
    deriving Eq

instance Binary Hash where
    put (Hash x) = put x
    get = Hash <$> get

hashBlob :: Str.ByteString -> Hash
hashBlob = Hash . Str.pack . SHA.hash . Str.unpack
