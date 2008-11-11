{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Udon.Dictionary 
    ( Dictionary
    , getDictionaryMap, makeDictionary
    )
where

import Data.Binary
import Udon.Types
import Udon.TypedBlob
import qualified Data.Map as Map

newtype Dictionary = Dictionary (Map.Map String TypedBlob)
    deriving Binary

getDictionaryMap :: Dictionary -> Map.Map String TypedBlob
getDictionaryMap (Dictionary m) = m

makeDictionary :: Map.Map String TypedBlob -> Dictionary
makeDictionary = Dictionary

instance Typed Dictionary where
    getType _ = TyContent ContentDictionary
