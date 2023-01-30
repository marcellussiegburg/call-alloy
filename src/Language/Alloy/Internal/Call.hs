{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Language.Alloy.Internal.Call
Copyright   : (c) Marcellus Siegburg, 2019 - 2021
License     : MIT

This module provides the basic internal functionality to retrieve the raw
results from calling Alloy.
It provides data types and functions to interact with Alloy.
-}
module Language.Alloy.Internal.Call (
  CallAlloyConfig (maxInstances, noOverflow, timeout),
  defaultCallAlloyConfig,
  getRawInstances,
  getRawInstancesWith,
  ) where

import qualified Data.ByteString                  as BS (
  hGetLine,
  intercalate,
  stripPrefix,
  )
import qualified Data.ByteString.Char8            as BS (unlines)

import Control.Concurrent (
  threadDelay,
  )
import Control.Concurrent.Async (
  concurrently,
  mapConcurrently_,
  wait,
  withAsync
  )
import Control.Concurrent.Extra         (Lock, newLock, withLock)
import Control.Exception                (IOException, bracket, catch)
import Control.Monad                    (unless, when)
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (unpack)
import Data.List                        (intercalate)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe)
import System.Exit                      (ExitCode (..))
import System.FilePath
  (searchPathSeparator)
import System.IO (
  BufferMode (..),
  Handle,
  hClose,
  hFlush,
  hIsEOF,
  hPutStr,
  hPutStrLn,
  hSetBuffering,
  stderr,
  )
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process (
  CreateProcess (..), StdStream (..), ProcessHandle,
  cleanupProcess,
  createProcess, proc, terminateProcess, waitForProcess,
  )

import Language.Alloy.RessourceNames (
  className, classPackage,
  )
import Language.Alloy.Ressources (
  alloyJar,
  commonsCliJar,
  slf4jJar,
  )
import Paths_call_alloy                 (getDataDir)

{-|
Configuration for calling alloy. These are:

 * maximal number of instances to retrieve ('Nothing' for all)
 * whether to not overflow when calculating numbers within Alloy
 * an timeout after which to forcibly kill Alloy
   (retrieving only instances that were returned before killing the process)
-}
data CallAlloyConfig = CallAlloyConfig {
  -- | maximal number of instances to retrieve ('Nothing' for all)
  maxInstances :: !(Maybe Integer),
  -- | whether to not overflow when calculating numbers within Alloy
  noOverflow   :: !Bool,
  -- | the time in microseconds after which to forcibly kill Alloy
  --   ('Nothing' for never)
  timeout      :: !(Maybe Int)
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

{-# NOINLINE outLock #-}
outLock :: Lock
outLock = unsafePerformIO newLock

putOutLn :: String -> IO ()
putOutLn = withLock outLock . putStrLn

putErrLn :: String -> IO ()
putErrLn = withLock outLock . hPutStrLn stderr

{-|
This function may be used to get all raw model instances for a given Alloy
specification. It calls Alloy via a Java interface and splits the raw instance
answers before returning the resulting list of raw instances.
-}
getRawInstances
  :: Maybe Integer
  -- ^ How many instances to return; 'Nothing' for all.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [ByteString]
getRawInstances maxIs = getRawInstancesWith defaultCallAlloyConfig {
  maxInstances = maxIs
  }

{-|
Creates an Alloy process using the given config.
-}
callAlloyWith
  :: CallAlloyConfig
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
callAlloyWith config = do
  classPath <- getClassPath
  let callAlloy = proc "java"
        $ ["-cp", classPath, classPackage ++ '.' : className,
           "-i", show $ fromMaybe (-1) $ maxInstances config]
        ++ ["-o" | not $ noOverflow config]
  createProcess callAlloy {
    std_out = CreatePipe,
    std_in  = CreatePipe,
    std_err = CreatePipe
  }

{-|
This function may be used to get all raw model instances for a given Alloy
specification. It calls Alloy via a Java interface and splits the raw instance
answers before returning the resulting list of raw instances.
Parameters are set using a 'CallAlloyConfig'.
-}
getRawInstancesWith
  :: CallAlloyConfig
  -- ^ The configuration to be used.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [ByteString]
getRawInstancesWith config content
  = bracket (callAlloyWith config) cleanupProcess $ \p -> do
  (Just hin, Just hout, Just herr, ph) <- return p
#ifndef mingw32_HOST_OS
  hSetBuffering hin NoBuffering
#endif
  let evaluateAlloy' = do
        hPutStr hin content
        hFlush hin
        hClose hin
      evaluateAlloy = catch evaluateAlloy' $ \e -> do
        let err = show (e :: IOException)
            warn = "Maybe not complete instance was sent to Alloy "
            explain = "(Are timeouts set? Make sure they are not too small!): "
        putErrLn ("Warning: " ++ warn ++ explain ++ err)
  withTimeout hin hout herr ph (timeout config) $ do
    (out, err) <- fst <$> concurrently
      (concurrently (getOutput hout) (getOutput herr))
      evaluateAlloy
    printContentOnError ph
    let err' = removeInfoLines err
    unless (null err') $ fail $ unpack $ BS.unlines err'
    return $ fmap (BS.intercalate "\n")
      $ filterLast ((/= partialInstance) . last)
      $ drop 1 $ splitOn [begin] out
  where
    begin :: ByteString
    begin = "---INSTANCE---"
    filterLast _ []     = []
    filterLast p x@[_]  = filter p x
    filterLast p (x:xs) = x:filterLast p xs
    getOutput' h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> BS.hGetLine h <*> getOutput h
    getOutput h = catch
      (getOutput' h)
      (\(_ :: IOException) -> return [partialInstance])
    printContentOnError ph = do
      code <- waitForProcess ph
      when (code == ExitFailure 1)
        $ putOutLn $ "Failed parsing the Alloy code:\n" <> content

partialInstance :: ByteString
partialInstance = "---PARTIAL_INSTANCE---"

{-|
Removes lines such as

@
[main] INFO kodkod.engine.config.Reporter - detecting symmetries ...
[main] INFO kodkod.engine.config.Reporter - detected 16 equivalence classes of atoms ...
[main] INFO kodkod.engine.config.Reporter - optimizing bounds and formula (breaking predicate symmetries, inlining, skolemizing) ...
[main] INFO kodkod.engine.config.Reporter - translating to boolean ...
[main] INFO kodkod.engine.config.Reporter - generating lex-leader symmetry breaking predicate ...
@

and

@
[main] WARN kodkod.engine.config.Reporter - Temporal formula: will be reduced to possibly unsound static version.
@

and

@
PARTIAL_INSTANCE
@

which seem to be appearing since Alloy-6.0.0
-}
removeInfoLines :: [ByteString] -> [ByteString]
removeInfoLines (x:xs)
  | Just _ <- BS.stripPrefix "[main] INFO" x
  = removeInfoLines xs
  | Just _ <- BS.stripPrefix "[main] WARN" x
  = removeInfoLines xs
  | x == partialInstance
  = removeInfoLines xs
removeInfoLines xs = xs

{-|
Start a new sub process that communicates with the worker process
if a timeout is provided.
Execution is aborted by closing all handles and
killing the underlying worker processes after the given amount of time
(if it has not finished by then).
The process will wait for the sub process to make the result available.

If the provided timeout is 'Nothing', evaluation happens without
scheduled interruption in the main thread.
-}
withTimeout
  :: Handle
  -- ^ the input handle (of the worker) to close
  -> Handle
  -- ^ the output handle (of the worker) to close
  -> Handle
  -- ^ the error handle (of the worker) to close
  -> ProcessHandle
  -- ^ the worker process handle
  -> Maybe Int
  -- ^ the timeout (Nothing if no timeout)
  -> IO a
  -- ^ some action interacting with the worker and its handles
  -> IO a
withTimeout _ _ _ _  Nothing  p = p
withTimeout i o e ph (Just t) p = withAsync p $ \a -> do
  threadDelay t
  mapConcurrently_ id [
    hClose e,
    hClose o,
    terminateProcess ph
    ]
  hClose i
  wait a

{-|
Get the class path of all files in the data directory.

Returns the class path.
-}
getClassPath :: IO FilePath
getClassPath =
  concatPaths <$> getDataDir <*> alloyJar <*> commonsCliJar <*> slf4jJar
  where
    concatPaths w x y z = intercalate
      [searchPathSeparator]
      [w, x, y, z]
