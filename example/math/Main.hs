{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Main where

import Yesod hiding (Number)

import Data.Aeson       hiding (Number)
import Data.Aeson.Types hiding (Number)

import Yesod.Except


-- | Type of operation to handle.
data OpType = OpTypePlus | OpTypeMinus

-- | How to parse 'OpType'.
instance FromJSON OpType where
  parseJSON (String str) =
    case str of
      "plus"  -> pure OpTypePlus
      "minus" -> pure OpTypeMinus
      _       -> fail "unknown operation"
  parseJSON invalid =
    typeMismatch "OpType" invalid

-- | Wrapper for a number.
newtype Number = Number { toInt :: Int }

-- | How to parse 'Number'.
instance FromJSON Number where
  parseJSON = fmap Number . parseJSON

-- | How to serialize 'Number'.
instance ToJSON Number where
  toJSON = toJSON . toInt


-- | Application data.
data App = App

-- | Application routes.
mkYesod "App" [parseRoutes|
/ MathServiceR POST
|]

-- | 'App' is an instance of 'Yesod'
instance Yesod App

-- | Math service handler.
postMathServiceR :: Handler Value
postMathServiceR =
  -- Wrap handler's body into `MonadError` instance which
  -- returns a json object after evaluation. This object is
  -- constructed automatically.
  runExceptV .
    -- Require json object to be present in request body.
    withJsonObject $ do
      -- Get type of opearation to handle.
      opType <- askValue "type"
      -- Get first argument.
      nmLeft <- toInt <$> askValue "left"
      -- Get second argument. 
      nmRight <- toInt <$> askValue "right"
      -- Return result.
      pure . Number $
        case opType of
          OpTypePlus  -> nmLeft + nmRight
          OpTypeMinus -> nmLeft - nmRight


-- | Just main.
main :: IO ()
main = do
  warp 3000 App
