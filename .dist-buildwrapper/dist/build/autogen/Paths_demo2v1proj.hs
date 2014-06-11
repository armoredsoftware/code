module Paths_demo2v1proj (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/users/paulkline/.cabal/bin"
libdir     = "/users/paulkline/.cabal/lib/demo2v1proj-0.1/ghc-7.6.3"
datadir    = "/users/paulkline/.cabal/share/demo2v1proj-0.1"
libexecdir = "/users/paulkline/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "demo2v1proj_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "demo2v1proj_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "demo2v1proj_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "demo2v1proj_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
