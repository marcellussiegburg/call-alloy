{-# LANGUAGE TemplateHaskell #-}
module Language.Alloy.Ressources (
  alloyJar,
  classFile,
  commonsCliJar,
  slf4jJar,
  ) where

import Data.ByteString                  (ByteString)
import Data.FileEmbed                   (embedFile)
import System.FilePath                  ((</>), (<.>))

import Language.Alloy.RessourceNames (
  alloyJarName, className, classPackage, commonsCliJarName, slf4jJarName,
  )

alloyJar :: ByteString
alloyJar = $(embedFile $ "bin" </> alloyJarName)

commonsCliJar :: ByteString
commonsCliJar = $(embedFile $ "bin" </> "commons-cli" </> commonsCliJarName)

slf4jJar :: ByteString
slf4jJar = $(embedFile $ "bin" </> "slf4j" </> slf4jJarName)

classFile :: ByteString
classFile = $(embedFile $ "bin" </> classPackage </> className <.> "class")
