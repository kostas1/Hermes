module Paths_Hermes (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/kostas/.cabal/bin"
libdir     = "/home/kostas/.cabal/lib/Hermes-0.0.0/ghc-7.0.4"
datadir    = "/home/kostas/.cabal/share/Hermes-0.0.0"
libexecdir = "/home/kostas/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Hermes_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Hermes_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Hermes_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Hermes_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
