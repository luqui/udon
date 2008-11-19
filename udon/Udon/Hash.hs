module Udon.Hash 
    ( Blob, Hash, showHash, binHashGet, binHashPut, hashBlob, hashBinary ) 
where

import qualified Data.ByteString.Lazy as Str
import qualified Data.Digest.SHA256 as SHA
import Data.Binary
import qualified Codec.Binary.Base16 as Base16

type Blob = Str.ByteString

newtype Hash = Hash Blob
    deriving (Eq,Ord)

showHash :: Hash -> String
showHash (Hash blob) = Base16.encode . Str.unpack $ blob

-- Not an instance of binary so that we can't accidentally write
-- a hash without referencing it in the Chunk structure.
binHashPut :: Hash -> Put
binHashPut (Hash x) = put x

binHashGet :: Get Hash
binHashGet = fmap Hash get

hashBlob :: Blob -> Hash
hashBlob = Hash . Str.pack . SHA.hash . Str.unpack

hashBinary :: (Binary a) => a -> Hash
hashBinary = hashBlob . encode
