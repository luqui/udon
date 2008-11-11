module Udon.Types 
    ( Type(..)
    , ContentType(..)
    )
where

import Data.Binary
import Control.Applicative

data Type
    = TyContent ContentType

data ContentType
    = ContentDictionary
    | ContentText

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
