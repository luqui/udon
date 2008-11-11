module Udon.HashFS where

import qualified Data.Digest.SHA2 as SHA2
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as Str
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Codec.Binary.Base64.String as Base64
import Control.Applicative
import Data.Binary

newtype Repo = Repo FilePath
newtype Ref = Ref Str.ByteString

instance Binary Ref where
    get = Ref <$> get
    put (Ref b) = put b

instance Show Ref where
    show r = "fromStr \"" ++ asciiHash r ++ "\""

-- This should really all be in a filesystem transaction monad.
-- It is horribly unsafe at the moment.

makeRepo :: FilePath -> IO Repo
makeRepo path = do
    createDirectory path
    createDirectory $ path ++ "/objects"  -- XXX unix-centric
    return $ Repo path

openRepo :: FilePath -> IO Repo
openRepo path = do
    exists <- doesDirectoryExist path
    if exists
        then return $ Repo path
        else fail $ "No such repository: " ++ path

closeRepo :: Repo -> IO ()
closeRepo _ = return ()

asciiHash :: Ref -> String
asciiHash (Ref bs) = map subst . filter (/= '\n') . Base64.encode . Char8.unpack $ bs
    where
    subst '/' = '_'
    subst x   = x

fromStr :: String -> Ref
fromStr = Ref . Char8.pack . Base64.decode . map subst
    where
    subst '_' = '/'
    subst x   = x

sha256 = Ref . Str.pack . SHA2.toOctets . SHA2.sha256 . Str.unpack

addBlob :: Repo -> Str.ByteString -> IO Ref
addBlob (Repo path) blob = do
    let hash = sha256 blob
    Str.writeFile (path ++ "/objects/" ++ asciiHash hash) blob
    return hash

getBlob :: Repo -> Ref -> IO (Maybe Str.ByteString)
getBlob (Repo path) hash = do
    let file = path ++ "/objects/" ++ asciiHash hash
    exists <- doesFileExist file
    if exists
        then Just <$> Str.readFile file
        else return Nothing

addObject :: (Binary b) => Repo -> b -> IO Ref
addObject repo = addBlob repo . encode

getObject :: (Binary b) => Repo -> Ref -> IO (Maybe b)
getObject repo ref = fmap decode <$> getBlob repo ref
