{-|
Module      : Language.Alloy.Call
Description : A simple library to call Alloy given a specification
Copyright   : (c) Marcellus Siegburg, 2019 - 2021
License     : MIT
Maintainer  : marcellus.siegburg@uni-due.de

This module provides basic functionality to interact with Alloy.
This library contains Alloy and an (internal) interface to interact with it.
These libraries will be placed into the user's directory during execution.
A requirement for this library to work is a Java Runtime Environment
(as it is required by Alloy).
-}
module Language.Alloy.Call (
  CallAlloyConfig (maxInstances, noOverflow, timeout),
  defaultCallAlloyConfig,
  existsInstance,
  getInstances,
  getInstancesWith,
  module Functions,
  module Types,
  ) where

import Language.Alloy.Functions         as Functions
import Language.Alloy.Internal.Call
import Language.Alloy.Parser            (parseInstance)
import Language.Alloy.Types             as Types
  (AlloyInstance, AlloySig, Entries, Object, Signature)

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
getInstancesWith config content =
  map (either (error . show) id . parseInstance)
  <$> getRawInstancesWith config content

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
