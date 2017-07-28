{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
module Handler.Answer where

import ClassyPrelude.Yesod hiding (Text, pack)
import Control.Monad.Except
import Data.Text (Text, pack)


-- * Runner for ExceptT handler.

newtype Answer a = Answer { toEither :: Either Text a }

instance ToJSON a => ToJSON (Answer a) where
  toJSON (Answer (Left msg)) = object
    [ "error"   .= True
    , "message" .= msg
    , "data"    .= Null
    ]
  toJSON (Answer (Right val)) = object
    [ "error"   .= False
    , "message" .= pack "OK"
    , "data"    .= val
    ]

answerToRequest
  :: ToJSON a
  => ExceptT Text (HandlerT site IO) a
  -> HandlerT site IO Value
answerToRequest request =
  runExceptT request >>= returnJson . Answer 
