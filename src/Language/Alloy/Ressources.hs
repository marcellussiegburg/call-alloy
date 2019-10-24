{-# LANGUAGE TemplateHaskell #-}
module Language.Alloy.Ressources (
  alloyJar,
  classFile,
  ) where

import Data.ByteString                  (ByteString)
import Data.FileEmbed                   (embedFile)
import System.FilePath                  ((</>), (<.>))

import Language.Alloy.RessourceNames    (alloyJarName, className, classPackage)

alloyJar :: ByteString
alloyJar = $(embedFile $ "bin" </> alloyJarName)

classFile :: ByteString
classFile = $(embedFile $ "bin" </> classPackage </> className <.> "class")
