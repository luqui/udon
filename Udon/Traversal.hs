module Udon.Traversal 
    ( Traversal
    , TraverseNode(..)
    , MarkRefs(..)
    )
where

import Udon.HashFS
import qualified Data.ByteString.Lazy as Str

type Traversal = [TraverseNode]

data TraverseNode
    = TraverseNode Ref (Str.ByteString -> Traversal)

class MarkRefs a where
    markRefs :: a -> Traversal
