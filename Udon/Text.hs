{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Udon.Text 
    ( Text, makeText, getText )
where

import qualified Data.ByteString.Lazy as Str
import Data.Binary
import Udon.Types

newtype Text = Text Str.ByteString
    deriving Binary

makeText :: Str.ByteString -> Text
makeText = Text

getText :: Text -> Str.ByteString
getText (Text s) = s

instance Typed Text where
    getType _ = TyContent ContentText
