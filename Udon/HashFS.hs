module Udon.HashFS 
    ( Repo, Ref
    , HashFS
    , makeRepo, openRepo, closeRepo
    , inRepo
    , fromStr
    , addBlob, getBlob
    , addObject, getObject
    , hashObject
    )
where

import qualified Data.Digest.SHA2 as SHA2
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as Str
import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Codec.Binary.Base64.String as Base64
import Control.Applicative
import Data.Binary
import Control.Monad.Reader

newtype Repo = Repo FilePath
newtype Ref = Ref Str.ByteString

newtype HashFS a = HashFS (ReaderT Repo IO a)
    deriving (Functor, Monad)

instance Binary Ref where
    get = Ref <$> get
    put (Ref b) = put b

instance Show Ref where
    show r = "fromStr \"" ++ asciiHash r ++ "\""

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

inRepo :: Repo -> HashFS a -> IO a
inRepo repo (HashFS m) = runReaderT m repo

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

addBlob :: Str.ByteString -> HashFS Ref
addBlob blob = HashFS $ do
    Repo path <- ask
    let hash = sha256 blob
    liftIO $ Str.writeFile (path ++ "/objects/" ++ asciiHash hash) blob
    return hash

getBlob :: Ref -> HashFS (Maybe Str.ByteString)
getBlob hash = HashFS $ do
    Repo path <- ask
    let file = path ++ "/objects/" ++ asciiHash hash
    exists <- liftIO $ doesFileExist file
    if exists
        then Just <$> liftIO (Str.readFile file)
        else return Nothing

addObject :: (Binary b) => b -> HashFS Ref
addObject = addBlob . encode

getObject :: (Binary b) => Ref -> HashFS (Maybe b)
getObject ref = fmap decode <$> getBlob ref

hashObject :: (Binary b) => b -> Ref
hashObject = sha256 . encode
