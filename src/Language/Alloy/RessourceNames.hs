module Language.Alloy.RessourceNames (
  alloyJarName,
  className,
  classPackage,
  commonsCliJarName,
  ) where

alloyJarName :: String
alloyJarName = "org.alloytools.alloy.dist.jar"

commonsCliJarName :: String
commonsCliJarName = "commons-cli-1.5.0.jar"

className :: String
className = "RunAlloy"

classPackage :: String
classPackage = "alloy"
