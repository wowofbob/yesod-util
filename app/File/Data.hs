{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
module File.Data where

import ClassyPrelude.Yesod
import Control.Monad.Except
import System.Directory
import System.FilePath

import qualified Data.ByteString as BS

import qualified Data.Text.Encoding       as Text
import qualified Data.Text.Encoding.Error as Text

import Yesod.Except.Json
import Yesod.Except.Wrappers


-- * Helpers.

-- | Read file contents, decode it as Utf8.
-- On error, replace invalid byte with utf8 replacement.
readUtf8File :: FilePath -> IO Text
readUtf8File =
  fmap (Text.decodeUtf8With Text.lenientDecode) . BS.readFile


-- | Write strict Text to a file as utf8.
writeUtf8File :: FilePath -> Text -> IO ()
writeUtf8File fp =
  BS.writeFile fp . Text.encodeUtf8


-- * Subsite definition.

-- | File subsite data.
data File = File

-- | Requirements for master site.
class YesodFile master where
  fileRootDir :: HandlerT master IO FilePath

-- | Alias for file subsite handler.
type FileHandler a =
  forall master . YesodFile master =>
    HandlerT File (HandlerT master IO) a


-- * Subsite auxiliary functions.

-- | Make absolute path from file root directory to supplied file by using it's
-- relative path.
mkAbsPath
  :: YesodFile master =>
    FilePath -> HandlerT File (HandlerT master IO) FilePath
mkAbsPath filePath = flip combine filePath <$> lift fileRootDir

-- | Ask file path from environment.
askFile
  :: YesodFile master =>
    JsonObjEnv (HandlerT File (HandlerT master IO)) FilePath
askFile = askValue "file" >>= lift . mkAbsPath

-- | Ask path to existing file from environment.
askExistingFile
  :: YesodFile master =>
    JsonObjEnv (HandlerT File (HandlerT master IO)) FilePath
askExistingFile = do
  fp <- askFile
  exists <- liftIO $ doesFileExist fp
  when (not exists) $ throwError "file does not exist"
  pure fp
  
-- | Ask file contents from environment.
askContents
  :: YesodFile master =>
    JsonObjEnv (HandlerT File (HandlerT master IO)) Text
askContents = askValue "text"
  

-- * Subsite handlers.

mkYesodSubData "File" [parseRoutes|
/ FileR DELETE GET PUT PATCH
|]

deleteFileR :: FileHandler Value
deleteFileR = withJsonObjEnv $ do
  askExistingFile >>= liftIO . removeFile

getFileR :: FileHandler Value
getFileR = withJsonObjEnv $ do
  askExistingFile >>= liftIO . readUtf8File

putFileR :: FileHandler Value
putFileR = withJsonObjEnv $ do
  path <- askFile
  text <- askContents
  liftIO $ writeUtf8File path text

patchFileR :: FileHandler Value
patchFileR = withJsonObjEnv $ do
  path <- askExistingFile
  text <- askContents
  liftIO $ writeUtf8File path text
