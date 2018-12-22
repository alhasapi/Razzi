{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
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

bindir     = "/home/yerima/nj/Fakr/.cabal-sandbox/bin"
libdir     = "/home/yerima/nj/Fakr/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2/Fakr-0.1.0.0"
dynlibdir  = "/home/yerima/nj/Fakr/.cabal-sandbox/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/yerima/nj/Fakr/.cabal-sandbox/share/x86_64-linux-ghc-8.0.2/Fakr-0.1.0.0"
libexecdir = "/home/yerima/nj/Fakr/.cabal-sandbox/libexec"
sysconfdir = "/home/yerima/nj/Fakr/.cabal-sandbox/etc"

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
