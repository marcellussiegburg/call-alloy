{-|
Module      : Language.Alloy.Call
Description : A simple library to call Alloy given a specification
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module provides basic functionality to interact with Alloy.
This library contains Alloy and an (internal) interface to interact with it.
These libraries will be placed into the users directory during execution.
A requirement for this library to work is a Java Runtime Environment
(as it is required by Alloy).
-}
{-# LANGUAGE CPP #-}
module Language.Alloy.Call (
  existsInstance,
  getInstances,
  module Parser
  ) where

import qualified Data.ByteString                  as BS (writeFile)

import qualified Language.Alloy.Parser            as Parser

import Control.Monad                    (unless)
import Data.Hashable                    (hash)
import Data.IORef                       (IORef, newIORef, readIORef)
import Data.List                        (intercalate)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe)
import System.Directory
  (XdgDirectory (..), createDirectory, doesFileExist, doesDirectoryExist,
   getTemporaryDirectory, getXdgDirectory)
import System.Directory.Internal        (setFileMode)
import System.Directory.Internal.Prelude
  (catch, isDoesNotExistError)
import System.Exit                      (ExitCode (..))
import System.FilePath
  ((</>), (<.>), searchPathSeparator, takeDirectory)
import System.IO                        (hClose, hGetLine, hIsEOF, hPutStr)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process
#if defined(mingw32_HOST_OS)
import System.Win32.Info                (getUserName)
#else
import System.Posix.User                (getLoginName)
#endif

import Language.Alloy.Parser            (AlloyInstance)
import Language.Alloy.RessourceNames    (alloyJarName, className, classPackage)
import Language.Alloy.Ressources        (alloyJar, classFile)

{-# NOINLINE mclassPath #-}
{-|
'IORef' for storing the class path.
-}
mclassPath :: IORef (Maybe FilePath)
mclassPath = unsafePerformIO (newIORef Nothing)

{-|
This function may be used to get all model instances for a given Alloy
specification. It calls Alloy via a Java interface and returns the raw instance
answers as list of 'String's.
-}
getInstances
  :: Maybe Integer
  -- ^ How many instances to return 'Nothing' for all.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [AlloyInstance]
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
  instas <- printContentOnError ph `seq`
    fmap (intercalate "\n") . drop 1 . splitOn [begin] <$> getWholeOutput hout
  return $ either (error . show) id . Parser.parseInstance <$> instas
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

{-|
Check if the class path was determined already, if so use it, otherwise call
'readClassPath'.

Returns the class path.
-}
getClassPath :: IO FilePath
getClassPath = do
  mclassPath' <- readIORef mclassPath
  maybe readClassPath return mclassPath'

fallbackToTempDir :: IO FilePath -> IO FilePath
fallbackToTempDir m = catch m $ \e ->
  if isDoesNotExistError e
  then do
    tmp    <- getTemporaryDirectory
#if defined(mingw32_HOST_OS)
    login  <- getUserName
#else
    login  <- getLoginName
#endif
    let tmpDir = tmp </> show (hash login) </> appName
    createUserDirectoriesIfMissing tmpDir
    return tmpDir
  else error $ show e

{-|
Read the class path version specified in the user directory, if it is not
current or if it does not exist, call 'createVersionFile'.

Returns the class path.
-}
readClassPath :: IO FilePath
readClassPath = do
  configDir <- fallbackToTempDir $ getXdgDirectory XdgConfig appName
  let versionFile = configDir </> "version"
  exists <- doesFileExist versionFile
  if exists
    then do
    version <- read <$> readFile versionFile
    unless (version == versionHash) $ createVersionFile configDir versionFile
    else createVersionFile configDir versionFile
  dataDir <- getXdgDirectory XdgData $ appName </> "dataDir"
  return $ dataDir ++ searchPathSeparator : dataDir </> alloyJarName

{-|
Create all library files within the users 'XdgDirectory' by calling
'createDataDir' then place the current version number into a configuration File.
-}
createVersionFile :: FilePath -> FilePath -> IO ()
createVersionFile configDir versionFile = do
  createDataDir
  createUserDirectoriesIfMissing configDir
  writeFile versionFile $ show versionHash

{-|
Create all library files within the users 'XdgDirectory' based on the source
files enclosed into this library (see also 'Language.Alloy.RessourceNames' and
'Language.Alloy.Ressources').
-}
createDataDir :: IO ()
createDataDir = do
  dataDir <- fallbackToTempDir $ getXdgDirectory XdgData $ appName </> "dataDir"
  createUserDirectoriesIfMissing $ dataDir </> classPackage
  BS.writeFile (dataDir </> classPackage </> className <.> "class") classFile
  BS.writeFile (dataDir </> alloyJarName) alloyJar

{-|
Creates user directories using the file permissions 700.
This function creates the specified directory and all its parent directories as
well (if they are also missing).
-}
createUserDirectoriesIfMissing :: FilePath -> IO ()
createUserDirectoriesIfMissing fp = do
  isDir <- doesDirectoryExist fp
  let parent = takeDirectory fp
  unless (isDir || parent == fp) $ do
    createUserDirectoriesIfMissing parent
    createDirectory fp
    setFileMode fp (7*8*8)

{-|
Check if there exists a model for the given specification. This function calls
Alloy retrieving one instance. If there is no such instance, it returns false.
This function calls 'getInstances'.
-}
existsInstance
  :: String
  -- ^ The Alloy specification which should be loaded.
  -> IO Bool
  -- ^ Whether there exists an instance (within the given scope)
existsInstance = fmap (not . null) . getInstances (Just 1)

{-|
The application name (used to store data in a specific directory.
-}
appName :: String
appName = "call-alloy"

{-# INLINE versionHash #-}
{-|
Used to determine possible source code and Alloy version changes across multiple
versions of this library.
-}
versionHash :: Int
versionHash = hash $ alloyHash + classFileHash
  where
    alloyHash = hash alloyJar
    classFileHash = hash classFile
