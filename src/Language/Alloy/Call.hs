{-|
Module      : Language.Alloy.Call
Description : A simple library to call Alloy given a specification
Copyright   : (c) Marcellus Siegburg, 2019
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module provides basic functionality to interact with Alloy.
This library contains Alloy and an (internal) interface to interact with it.
These libraries will be placed into the user's directory during execution.
A requirement for this library to work is a Java Runtime Environment
(as it is required by Alloy).
-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Alloy.Call (
  CallAlloyConfig (maxInstances, noOverflow),
  defaultCallAlloyConfig,
  existsInstance,
  getInstances,
  getInstancesWith,
  module Functions,
  module Types,
  ) where

import qualified Data.ByteString                  as BS
  (hGetLine, intercalate, writeFile)
import qualified Data.ByteString.Char8            as BS (unlines)

import Control.Concurrent
  (forkIO, killThread, newEmptyMVar, putMVar, takeMVar)
import Control.Lens.Internal.ByteString (unpackStrict8)
import Control.Monad                    (unless)
import Data.ByteString                  (ByteString)
import Data.Hashable                    (hash)
import Data.IORef                       (IORef, newIORef, readIORef)
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
import System.IO
  (BufferMode (..), hClose, hFlush, hIsEOF, hPutStr, hSetBuffering)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process
  (CreateProcess (..), StdStream (..), createProcess, proc, waitForProcess)
#if defined(mingw32_HOST_OS)
import System.Win32.Info                (getUserName)
#else
import System.Posix.User                (getLoginName)
#endif

import Language.Alloy.Functions         as Functions
import Language.Alloy.Parser            (parseInstance)
import Language.Alloy.RessourceNames (
  alloyJarName, className, classPackage, commonsCliJarName
  )
import Language.Alloy.Ressources        (alloyJar, classFile, commonsCliJar)
import Language.Alloy.Types             as Types
  (AlloyInstance, AlloySig, Entries, Object, Signature)

{-|
Configuration for calling alloy. These are:

 * maximal number of instances to retrieve ('Nothing' for all)
 * wheather to not overflow when calculating numbers within Alloy
-}
data CallAlloyConfig = CallAlloyConfig {
  -- | maximal number of instances to retrieve ('Nothing' for all)
  maxInstances :: Maybe Integer,
  -- | wheather to not overflow when calculating numbers within Alloy
  noOverflow   :: Bool
  }

{-|
Default configuration for calling Alloy. Defaults to:

 * retrieve all instances
 * do not overflow
-}
defaultCallAlloyConfig :: CallAlloyConfig
defaultCallAlloyConfig = CallAlloyConfig {
  maxInstances = Nothing,
  noOverflow   = True
  }

{-# NOINLINE mclassPath #-}
{-|
'IORef' for storing the class path.
-}
mclassPath :: IORef (Maybe FilePath)
mclassPath = unsafePerformIO (newIORef Nothing)

{-|
This function may be used to get all model instances for a given Alloy
specification. It calls Alloy via a Java interface and parses the raw instance
answers before returning the resulting list of 'AlloyInstance's.
-}
getInstances
  :: Maybe Integer
  -- ^ How many instances to return; 'Nothing' for all.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [AlloyInstance]
getInstances maxIs = getInstancesWith defaultCallAlloyConfig {
  maxInstances = maxIs
  }

{-|
This function may be used to get all model instances for a given Alloy
specification. It calls Alloy via a Java interface and parses the raw instance
answers before returning the resulting list of 'AlloyInstance's.
Parameters are set using a 'CallAlloyConfig'.
-}
getInstancesWith
  :: CallAlloyConfig
  -- ^ The configuration to be used.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [AlloyInstance]
getInstancesWith config content = do
  classPath <- getClassPath
  let callAlloy = proc "java"
        $ ["-cp", classPath, classPackage ++ '.' : className,
           "-i", show $ fromMaybe (-1) $ maxInstances config]
        ++ ["-o" | not $ noOverflow config]
  (Just hin, Just hout, Just herr, ph) <-
    createProcess callAlloy {
        std_out = CreatePipe,
        std_in  = CreatePipe,
        std_err = CreatePipe
      }
  pout <- listenForOutput hout
  perr <- listenForOutput herr
#ifndef mingw32_HOST_OS
  hSetBuffering hin NoBuffering
#endif
  hPutStr hin content
  hFlush hin
  hClose hin
  out <- getOutput pout
  err <- getOutput perr
  printContentOnError ph
  unless (null err) $ fail $ unpackStrict8 $ BS.unlines err
  let instas = fmap (BS.intercalate "\n") $ drop 1 $ splitOn [begin] out
  return $ either (error . show) id . parseInstance <$> instas
  where
    begin :: ByteString
    begin = "---INSTANCE---"
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> BS.hGetLine h <*> getWholeOutput h
    printContentOnError ph = do
      code <- waitForProcess ph
      unless (code == ExitSuccess)
        $ putStrLn $ "Failed parsing the Alloy code:\n" <> content
    listenForOutput h = do
      mvar <- newEmptyMVar
      pid <- forkIO $ getWholeOutput h >>= putMVar mvar
      return (pid, mvar)
    getOutput (pid, mvar) = do
      output <- takeMVar mvar
      killThread pid
      return output

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
    ++ searchPathSeparator : dataDir </> commonsCliJarName

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
  BS.writeFile (dataDir </> commonsCliJarName) commonsCliJar

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
#ifndef mingw32_HOST_OS
    setFileMode fp (7*8*8)
#endif

{-|
Check if there exists a model for the given specification. This function calls
Alloy retrieving one instance. If there is no such instance, it returns 'False'.
This function calls 'getInstances'.
-}
existsInstance
  :: String
  -- ^ The Alloy specification which should be loaded.
  -> IO Bool
  -- ^ Whether there exists an instance (within the relevant scope).
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
versionHash = hash $ alloyHash + commonsCliHash + classFileHash
  where
    alloyHash = hash alloyJar
    commonsCliHash = hash commonsCliJar
    classFileHash = hash classFile
