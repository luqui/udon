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
            paddyn <- makeDynRef fsdb padType (makeExtRef Map.empty)
            exp <- exportDyn fsdb paddyn
            newFile ["ROOTPAD"] (encode exp)

rootPadOp :: (Pad -> FST (a, Maybe Pad)) -> FST a
rootPadOp f = do
    exp <- decode <$> UdonShell.FST.readFile ["ROOTPAD"]
    paddyn <- fromJust <$> readExportDyn fsdb exp
    let pad = fromJust $ dynRefToExtRef padType paddyn
    (ret,mpad') <- f =<< runExt fsdb (deref pad)
    case mpad' of
        Nothing -> return ret
        Just pad' -> do
            paddyn' <- makeDynRef fsdb padType (makeExtRef pad')
            exp' <- exportDyn fsdb paddyn'
            newFile ["ROOTPAD"] (encode exp')
            return ret

cmdLet :: [String] -> IO ()
cmdLet [varname, filename] = do
    dir <- fstDir
    contents <- Str.readFile filename
    runFST dir . rootPadOp $ \pad -> do
        dat <- makeDynRef fsdb fileType (makeExtRef contents)
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
        let mdyn = Map.lookup varname pad
        r <- case mdyn of
            Nothing -> return (Left $ "No such root pad entry " ++ varname)
            Just dyn -> do
                case dynRefToExtRef fileType dyn of
                    Nothing -> return (Left "Pad entry does not have correct type")
                    Just extref -> do
                        Right <$> runExt fsdb (deref extref)
        return (r, Nothing)
    case mcontents of
        Left errmsg -> hPutStrLn stderr $ "*** Error: " ++ errmsg ++ "\n"
        Right content -> Str.putStr content


main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Commands: init, let, ls, show"
        ("init":cmdargs) -> cmdInit cmdargs
        ("let":cmdargs) -> cmdLet cmdargs
        ("ls":cmdargs) -> cmdLs cmdargs
        ("show":cmdargs) -> cmdShow cmdargs
        _ -> putStrLn "Unknown command"
