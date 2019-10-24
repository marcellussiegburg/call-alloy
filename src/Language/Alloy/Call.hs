module Language.Alloy.Call (
  existsInstance,
  getInstances,
  ) where

import qualified Data.ByteString                  as BS (writeFile)

import Control.Monad                    (unless)
import Data.Hashable                    (hash)
import Data.IORef                       (IORef, newIORef, readIORef)
import Data.List                        (intercalate)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe)
import System.Directory
  (XdgDirectory (..), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.Exit                      (ExitCode (..))
import System.FilePath                  ((</>), (<.>), searchPathSeparator)
import System.IO                        (hClose, hGetLine, hIsEOF, hPutStr)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process

import Language.Alloy.RessourceNames    (alloyJarName, className, classPackage)
import Language.Alloy.Ressources        (alloyJar, classFile)

mclassPath :: IORef (Maybe FilePath)
{-# NOINLINE mclassPath #-}
mclassPath = unsafePerformIO (newIORef Nothing)

getInstances :: Maybe Integer -> String -> IO [String]
getInstances maxInstances content = do
  classPath <- getClassPath
  let callAlloy = proc "java"
        ["-cp", classPath, classPackage ++ '.' : className,
         show $ fromMaybe (-1) maxInstances]
  (Just hin, Just hout, Just herr, ph) <-
    createProcess callAlloy {
        std_out = CreatePipe,
        std_in  = CreatePipe,
        std_err = CreatePipe
      }
  hPutStr hin content
  hClose hin
  printCallErrors herr
  printContentOnError ph `seq`
    fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  where
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> hGetLine h <*> getWholeOutput h
    printContentOnError ph = do
      code <- waitForProcess ph
      unless (code == ExitSuccess)
        $ fail $ "Failed parsing your file:\n" <> content
    printCallErrors err = do
      errors <- getWholeOutput err
      unless (null errors) $ fail $ intercalate "\n" errors

getClassPath :: IO FilePath
getClassPath = do
  mclassPath' <- readIORef mclassPath
  maybe readClassPath return mclassPath'

readClassPath :: IO FilePath
readClassPath = do
  configDir <- getXdgDirectory XdgConfig appName
  let versionFile = configDir </> "version"
  exists <- doesFileExist versionFile
  if exists
    then do
    version <- read <$> readFile versionFile
    unless (version == versionHash) $ createVerionFile configDir versionFile
    else createVerionFile configDir versionFile
  dataDir <- getXdgDirectory XdgData $ appName </> "dataDir"
  return $ dataDir ++ searchPathSeparator : dataDir </> alloyJarName

createVerionFile :: FilePath -> FilePath -> IO ()
createVerionFile configDir versionFile = do
  createDataDir
  createDirectoryIfMissing True configDir
  writeFile versionFile $ show versionHash

createDataDir :: IO ()
createDataDir = do
  dataDir <- getXdgDirectory XdgData $ appName </> "dataDir"
  createDirectoryIfMissing True $ dataDir </> classPackage
  BS.writeFile (dataDir </> classPackage </> className <.> "class") classFile
  BS.writeFile (dataDir </> alloyJarName) alloyJar

existsInstance :: String -> IO Bool
existsInstance = fmap (not . null) . getInstances (Just 1)

appName :: String
appName = "call-alloy"

{-# INLINE versionHash #-}
versionHash :: Int
versionHash = hash $ alloyHash + classFileHash
  where
    alloyHash = hash alloyJar
    classFileHash = hash classFile
