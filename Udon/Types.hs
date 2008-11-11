module Udon.Types 
    ( Typed(..)
    , Type(..)
    , ContentType(..)
    )
where

import Data.Binary
import Control.Applicative
import qualified Data.ByteString as Str

class (Binary a) => Typed a where
    getType :: a -> Type

data Type
    = TyContent ContentType
    deriving (Eq)

data ContentType
    = ContentDictionary
    | ContentText
    deriving (Eq)

instance Binary Type where
    get = TyContent <$> get
    put (TyContent c) = put c

instance Binary ContentType where
    get = do
        r <- getWord8
        case r of
            0 -> return ContentDictionary
            1 -> return ContentText
            _ -> fail $ "Unknown tag for ContentType: " ++ show r
    put ContentDictionary = putWord8 0
    put ContentText       = putWord8 1
