{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Jump (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
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
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/egor/.cabal/bin"
libdir     = "/home/egor/.cabal/lib/x86_64-linux-ghc-8.6.5/Jump-0.1.0.0-903II7vku5nHTB6EKZOPXc-Jump"
dynlibdir  = "/home/egor/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/egor/.cabal/share/x86_64-linux-ghc-8.6.5/Jump-0.1.0.0"
libexecdir = "/home/egor/.cabal/libexec/x86_64-linux-ghc-8.6.5/Jump-0.1.0.0"
sysconfdir = "/home/egor/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Jump_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Jump_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Jump_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Jump_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Jump_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Jump_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
