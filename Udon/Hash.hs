module Udon.Hash 
    ( Blob, Hash, hashBlob ) 
where

import qualified Data.ByteString.Lazy as Str
import qualified Data.Digest.SHA256 as SHA
import Data.Binary
import Control.Applicative

type Blob = Str.ByteString

newtype Hash = Hash Blob
    deriving Eq

instance Binary Hash where
    put (Hash x) = put x
    get = Hash <$> get

hashBlob :: Blob -> Hash
hashBlob = Hash . Str.pack . SHA.hash . Str.unpack
