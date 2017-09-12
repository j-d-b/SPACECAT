{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_spacecat (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/jbrady/Library/Haskell/bin"
libdir     = "/Users/jbrady/Library/Haskell/ghc-8.0.1-x86_64/lib/spacecat-0.1.1.0"
datadir    = "/Users/jbrady/Library/Haskell/share/ghc-8.0.1-x86_64/spacecat-0.1.1.0"
libexecdir = "/Users/jbrady/Library/Haskell/libexec"
sysconfdir = "/Users/jbrady/Library/Haskell/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "spacecat_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "spacecat_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "spacecat_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "spacecat_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "spacecat_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
