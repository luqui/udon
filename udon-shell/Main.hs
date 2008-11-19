module Main where

import Prelude hiding (log)
import UdonShell.FST
import UdonShell.FSDB
import Udon.API
import System.Environment
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad (when)
import Data.Binary
import System.IO
import System.Directory
import qualified Data.ByteString.Lazy as Str
import qualified Data.Map as Map

type Pad = Map.Map String DynRef

padType = makeDynType (undefined :: Pad)
fileType = makeDynType (undefined :: Str.ByteString)

fstDir :: IO FilePath
fstDir = do
    dir <- getEnv "UDON_DIR"
    when (null dir) $ fail "Environment variable UDON_DIR not set"
    return dir

log :: String -> IO a -> IO a
log msg io = putStrLn msg >> io

cmdInit :: [String] -> IO ()
cmdInit [] = do
    dir <- fstDir
    log ("Creating udon repository at " ++ dir) $ do
        makeFSTDir dir
        runFST dir $ do
            let paddyn = extRefToDynRef padType (makeExtRef Map.empty)
            exp <- exportDyn fsdb paddyn
            newFile ["root.pad"] (encode exp)

rootPadOp :: (Pad -> Ext (a, Maybe Pad)) -> FST a
rootPadOp f = do
    exp <- decode <$> UdonShell.FST.readFile ["root.pad"]
    paddyn <- fromJust <$> readExportRef fsdb exp
    let pad = fromJust $ dynRefToExtRef padType paddyn
    (ret,mpad') <- runExt fsdb (f =<< deref pad)
    case mpad' of
        Nothing -> return ret
        Just pad' -> do
            let paddyn' = extRefToDynRef padType (makeExtRef pad')
            exp' <- exportDyn fsdb paddyn'
            newFile ["root.pad"] (encode exp')
            return ret

cmdLet :: [String] -> IO ()
cmdLet [varname, filename] = do
    dir <- fstDir
    contents <- Str.readFile filename
    runFST dir . rootPadOp $ \pad -> do
        let dat = extRefToDynRef fileType (makeExtRef contents)
        return ((), Just $ Map.insert varname dat pad)

cmdLs :: [String] -> IO ()
cmdLs [] = do
    dir <- fstDir
    list <- runFST dir . rootPadOp $ \pad -> return (Map.keys pad, Nothing)
    mapM_ putStrLn list

cmdShow :: [String] -> IO ()
cmdShow [varname] = do
    dir <- fstDir
    mcontents <- runFST dir . rootPadOp $ \pad -> do
        r <- case Map.lookup varname pad of
            Nothing -> return (Left $ "No such root pad entry " ++ varname)
            Just dyn -> do
                case dynRefToExtRef fileType dyn of
                    Nothing -> return (Left "Pad entry does not have correct type")
                    Just extref -> do
                        Right <$> deref extref
        return (r, Nothing)
    case mcontents of
        Left errmsg -> hPutStrLn stderr $ "*** Error: " ++ errmsg ++ "\n"
        Right content -> Str.putStr content

cmdGC :: [String] -> IO ()
cmdGC [] = do
    dir <- fstDir
    runFST dir $ do
        exp <- decode <$> UdonShell.FST.readFile ["root.pad"]
        gcCollect [exp]


main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Commands: init, let, ls, show, gc"
        ("init":cmdargs) -> cmdInit cmdargs
        ("let":cmdargs) -> cmdLet cmdargs
        ("ls":cmdargs) -> cmdLs cmdargs
        ("show":cmdargs) -> cmdShow cmdargs
        ("gc":cmdargs) -> cmdGC cmdargs
        _ -> putStrLn "Unknown command"
