{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Yesod
import System.Directory

import File
import File.Data

import Yesod.Except.Json
import Yesod.Except.Wrappers

data App = App
  { appFileRootDir :: FilePath
  }


-- File subsite data is empty, so no need to store it in App.
appFile :: App -> File
appFile _ = File

-- Implement File subsite requirements.
instance YesodFile App where
  fileRootDir = appFileRootDir <$> getYesod

-- App routes.
mkYesod "App" [parseRoutes|
/ HomeR GET
/file AppFileR File appFile
/echo EchoR POST
|]


instance Yesod App

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]


data Echo = Echo
  { echoText :: Text
  }

instance ToJSON Echo where
  toJSON e = object
    [ "text" .= echoText e
    ]

postEchoR :: Handler Value
postEchoR = runExceptVWithObject $ askValue "text" >>= pure . Echo
  

main :: IO ()
main = do
  
  -- Initialize App.
  let app = App
              { appFileRootDir = "_junk/file"
              }
  
  -- Make directory for File subsite if it's missing.
  createDirectoryIfMissing True (appFileRootDir app)
  
  -- Launch.
  warp 3000 app
