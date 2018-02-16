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
  

-- * Subsite handlers.

mkYesodSubData "File" [parseRoutes|
/ FileR DELETE GET PUT PATCH
|]


-- | Less general verison of 'FileHandler'. 
type FileHandler' master = HandlerT File (HandlerT master IO)

askFile
  ::
  ( YesodFile master
  , MonadTrans m, MonadTrans mr, HasObject r
  , Monad (m (FileHandler' master))
  , MonadReader r (mr (m (FileHandler' master)))
  , MonadError Text (mr (m (FileHandler' master)))
  ) => mr (m (FileHandler' master)) FilePath
askFile = do
  fileName <- askValue "file"
  filePath <- lift . lift . mkAbsPath $ fileName
  pure filePath
  
  
guardFileExists :: (MonadIO m, MonadError Text m) => FilePath -> m FilePath
guardFileExists fp = do
  exists <- liftIO $ doesFileExist fp
  when (not exists) $ throwError "file does not exist"
  pure fp

askExistingFile
  ::
  ( YesodFile master
  , MonadTrans m, MonadTrans mr, HasObject r
  , Monad (m (FileHandler' master))
  , MonadReader r (mr (m (FileHandler' master)))
  , MonadError Text (mr (m (FileHandler' master)))
  , MonadIO (mr (m (FileHandler' master)))
  ) => mr (m (FileHandler' master)) FilePath
askExistingFile = do
  filePath <- askFile
  guardFileExists filePath
  pure filePath


deleteFileR :: FileHandler Value
deleteFileR = runExceptVWithObject $ do
  askExistingFile >>= liftIO . removeFile
  
getFileR :: FileHandler Value
getFileR = runExceptVWithObject $ do
  askExistingFile >>= liftIO . readUtf8File
  
putFileR :: FileHandler Value
putFileR = runExceptVWithObject $ do
  filePath <- askFile
  contents <- askValue "text"
  liftIO $ writeUtf8File filePath contents
  
patchFileR :: FileHandler Value
patchFileR = runExceptVWithObject $ do
  filePath <- askExistingFile
  contents <- askValue "text"
  liftIO $ writeUtf8File filePath contents
