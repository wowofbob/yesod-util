{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
module File.Data where

import ClassyPrelude.Yesod
import System.FilePath


-- | File subsite data.
data File = File

-- | Requirements for master site.
class YesodFile master where
  fileRootDir :: HandlerT master IO FilePath

-- | Alias for file subsite handler.
type FileHandler a =
  forall master . YesodFile master =>
    HandlerT File (HandlerT master IO) a

-- | Make absolute path from file root directory to supplied file by using it's
-- relative path.
mkAbsPath :: FilePath -> FileHandler FilePath
mkAbsPath filePath = flip combine filePath <$> lift fileRootDir
