module UdonShell.FST 
    ( FST, makeFSTDir, runFST
    , Path, newFile, checkFile, readFile, deleteFile
    )
where

import Prelude hiding (readFile)
import System.Directory
import Control.Monad (when)
import Control.Monad.Reader
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as Str
import qualified Data.ByteString as StrStrict

-- Filesystem transactions
-- (an incorrect implementation, but provides the abstraction to replace it
-- with a correct one)


type FSTInternal = ReaderT FilePath IO
newtype FST a = FST (FSTInternal a)
    deriving (Functor, Monad)

makeFSTDir :: FilePath -> IO ()
makeFSTDir path = createDirectory path

runFST :: FilePath -> FST a -> IO a
runFST path (FST r) = do
    exists <- doesDirectoryExist path
    when (not exists) $ 
        fail $ "Directory " ++ path ++ " does not exist"
    runReaderT r path

type Path = [String]

makePath :: Path -> FSTInternal FilePath
makePath path = do
    prefix <- ask
    let dir = prefix ++ intercalate "/" (init path)
    liftIO $ createDirectoryIfMissing True dir
    return $ dir ++ last path

getPath :: Path -> FSTInternal FilePath
getPath path = do
    prefix <- ask
    return $ prefix ++ intercalate "/" path

newFile :: Path -> Str.ByteString -> FST ()
newFile path content = FST $ do
    file <- makePath path
    liftIO $ Str.writeFile file content

checkFile :: Path -> FST Bool
checkFile path = FST $ do
    liftIO . doesFileExist =<< getPath path

readFile :: Path -> FST Str.ByteString
readFile path = FST $ do
    file <- getPath path
    fmap (Str.pack . StrStrict.unpack) . liftIO . StrStrict.readFile $ file

deleteFile :: Path -> FST ()
deleteFile path = FST $ do
    liftIO . removeFile =<< getPath path
