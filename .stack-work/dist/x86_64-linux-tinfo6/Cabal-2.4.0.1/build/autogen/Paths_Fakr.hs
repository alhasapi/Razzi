{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_Fakr (
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

bindir     = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/bin"
libdir     = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/lib/x86_64-linux-ghc-8.6.5/Fakr-0.1.0.0-JA6n3z9UEB1E2Eadn3nJPi"
dynlibdir  = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/share/x86_64-linux-ghc-8.6.5/Fakr-0.1.0.0"
libexecdir = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/libexec/x86_64-linux-ghc-8.6.5/Fakr-0.1.0.0"
sysconfdir = "/home/emerson/workspace/dev/fakr/.stack-work/install/x86_64-linux-tinfo6/22ecde2e3bb034b9090b548261e6fcc471c4291707dea4e477a6a704a7c45dbf/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Fakr_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Fakr_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Fakr_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Fakr_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Fakr_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Fakr_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
