{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_text (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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
version = Version [2,1,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/bin"
libdir     = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/lib"
dynlibdir  = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/lib"
datadir    = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/share"
libexecdir = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/libexec"
sysconfdir = "/home/nasir/.local/state/cabal/store/ghc-9.8.4-65e9/text-2.1.3-19e91e11662417baa752121394d705e83775341f260daec6cb1269120f7eda30/etc"

getBinDir     = catchIO (getEnv "text_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "text_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "text_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "text_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "text_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "text_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
