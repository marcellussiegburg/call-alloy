{-# LANGUAGE TemplateHaskell #-}
module Language.Alloy.Ressources (
  alloyJar,
  classFile,
  commonsCliJar,
  ) where

import Data.ByteString                  (ByteString)
import Data.FileEmbed                   (embedFile)
import System.FilePath                  ((</>), (<.>))

import Language.Alloy.RessourceNames (
  alloyJarName, className, classPackage, commonsCliJarName
  )

alloyJar :: ByteString
alloyJar = $(embedFile $ "bin" </> alloyJarName)

commonsCliJar :: ByteString
commonsCliJar = $(embedFile $ "bin" </> "commons-cli" </> commonsCliJarName)

classFile :: ByteString
classFile = $(embedFile $ "bin" </> classPackage </> className <.> "class")
