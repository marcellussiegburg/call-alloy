{-# LANGUAGE ScopedTypeVariables #-}
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
  CallAlloyConfig (maxInstances, noOverflow, timeout),
  defaultCallAlloyConfig,
  existsInstance,
  getInstances,
  getInstancesWith,
  module Functions,
  module Types,
  ) where

import qualified Data.ByteString                  as BS
  (hGetLine, intercalate, isSuffixOf, writeFile)
import qualified Data.ByteString.Char8            as BS (unlines)

import Control.Concurrent (
  ThreadId,
  forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay,
  )
import Control.Exception                (IOException)
import Control.Lens.Internal.ByteString (unpackStrict8)
import Control.Monad                    (unless, void, when)
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
  (BufferMode (..), Handle, hClose, hFlush, hIsEOF, hPutStr, hSetBuffering)
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process (
  CreateProcess (..), StdStream (..), ProcessHandle,
  createProcess, proc, terminateProcess, waitForProcess,
  )
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
 * an timeout after which to forcibly kill Alloy
   (retrieving only instances that were returned before killing the process)
-}
data CallAlloyConfig = CallAlloyConfig {
  -- | maximal number of instances to retrieve ('Nothing' for all)
  maxInstances :: Maybe Integer,
  -- | wheather to not overflow when calculating numbers within Alloy
  noOverflow   :: Bool,
  -- | the time in microseconds after which to forcibly kill Alloy
  --   ('Nothing' for never)
  timeout      :: Maybe Int
  }

{-|
Default configuration for calling Alloy. Defaults to:

 * retrieve all instances
 * do not overflow
-}
defaultCallAlloyConfig :: CallAlloyConfig
defaultCallAlloyConfig = CallAlloyConfig {
  maxInstances = Nothing,
  noOverflow   = True,
  timeout      = Nothing
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
  maybe (return ()) (void . startTimeout hin hout herr ph) $ timeout config
  out <- getOutput pout
  err <- getOutput perr
  printContentOnError ph
  unless (null err) $ fail $ unpackStrict8 $ BS.unlines err
  let instas = fmap (BS.intercalate "\n") $ drop 1 $ splitOn [begin] out
  let finstas = filterLast (not . (partialInstance `BS.isSuffixOf`)) instas
  return $ either (error . show) id . parseInstance <$> finstas
  where
    begin :: ByteString
    begin = "---INSTANCE---"
    partialInstance :: ByteString
    partialInstance = "---PARTIAL_INSTANCE---"
    filterLast _ []     = []
    filterLast p x@[_]  = filter p x
    filterLast p (x:xs) = x:filterLast p xs
    getWholeOutput h = do
      eof <- hIsEOF h
      if eof
        then return []
      else catch
        ((:) <$> BS.hGetLine h <*> getWholeOutput h)
        (\(_ :: IOException) -> return [partialInstance])
    printContentOnError ph = do
      code <- waitForProcess ph
      when (code == ExitFailure 1)
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
Start a new process that aborts execution by closing all handles and
killing the processes after the given amount of time.
-}
startTimeout
  :: Handle
  -- ^ the input handle to close
  -> Handle
  -- ^ the output handle to close
  -> Handle
  -- ^ the error handle to close
  -> ProcessHandle
  -- ^ the main process handle
  -> Int -> IO ThreadId
startTimeout i o e ph t = forkIO $ do
  threadDelay t
  void $ forkIO $ hClose e
  void $ forkIO $ hClose o
  terminateProcess ph
  hClose i

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
