{-# LANGUAGE PatternGuards #-}

module Main where

import Udon.HashFS
import Udon.Dictionary
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as Str
import Data.Binary
import System
import System.IO
import System.Environment
import System.Directory
import Control.Monad.Reader
import Control.Applicative

data Env =
    Env { envRepo :: Repo
        , envRoot :: Ref
        , envHome :: FilePath
        }

type Shell = ReaderT Env IO
type Command = [String] -> Shell (Maybe Ref)


commands :: [(String, Command)]
commands = [ "ls" --> listRoot ]
    where
    infix 1 -->
    (-->) = (,)

    listRoot args = do
        env <- ask
        Just dict <- liftIO $ inRepo (envRepo env) $ getObject (envRoot env)
        forM_ (Map.assocs $ getDictionaryMap dict) $ \(k,v) -> do
            liftIO $ putStrLn k
        return Nothing

main :: IO ()
main = do
    envt <- getEnvironment
    env <- case lookup "UDON_HOME" envt of
        Just path -> openRepository path `catch` \_ -> do
                         hPutStrLn stderr $ "Creating new repository in " ++ path
                         newRepository path
        Nothing -> fail "UDON_HOME not set"
    
    args <- getArgs
    newroot <- case args of
        [] -> fail "No command given"
        (name:args) | Just cmd <- lookup name commands -> runReaderT (cmd args) env
                    | otherwise                        -> fail "Command not found"
   
    case newroot of
        Just r -> Str.writeFile (envHome env ++ "/root.ref") (encode r)
        Nothing -> return ()

newRepository :: FilePath -> IO Env
newRepository path = do
    createDirectory path
    repo <- makeRepo (path ++ "/udon")
    rootref <- inRepo repo $ addObject (makeDictionary Map.empty)
    Str.writeFile (path ++ "/root.ref") (encode rootref)
    return $ Env { envRepo = repo, envRoot = rootref, envHome = path }

openRepository :: FilePath -> IO Env
openRepository path = do
    repo <- openRepo (path ++ "/udon")
    rootref <- decode <$> Str.readFile (path ++ "/root.ref")
    return $ Env { envRepo = repo, envRoot = rootref, envHome = path }
