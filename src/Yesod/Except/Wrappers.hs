{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Yesod.Except.Wrappers
( ExceptV(..)
, runExceptV
, runExceptVWithObject
, ExceptM(..)
, runExceptM
, runExceptMWithObject
) where

import ClassyPrelude.Yesod hiding (Text, pack)

import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import Text.Blaze

import Yesod.Except.Json


-- | Same as 'ExceptT' but 'run' calls 'setMessage' on 'Left'.
newtype ExceptM m a = ExceptM { unExceptM :: ExceptT Text m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError Text
    , MonadThrow
    )

instance MonadTrans ExceptM where
  lift = ExceptM . lift

instance (MonadBase IO m) => MonadBase IO (ExceptM m) where
  liftBase = lift . liftBase

instance (MonadResource m) => MonadResource (ExceptM m) where 
  liftResourceT = lift . liftResourceT

instance (MonadHandler m) => MonadHandler (ExceptM m) where
  type HandlerSite (ExceptM m) = HandlerSite m
  liftHandlerT = lift . liftHandlerT


-- | Run 'ExceptT'. On error, call 'setMessage'. Otherwise, ignore result.
exceptToMessage :: (ToMarkup t, MonadHandler m) => ExceptT t m a -> m ()
exceptToMessage f = do
  eres <- runExceptT f
  case eres of
    Left  msg -> setMessage . toHtml $ msg
    Right _   -> pure ()

-- | Run 'ExceptM'. As a side effect, set message in session on error.
runExceptM :: (MonadHandler m) => ExceptM m a -> m ()
runExceptM = exceptToMessage . unExceptM

-- | Run environment with json object and set message in session on error.
runExceptMWithObject
  :: (MonadHandler m)
    => ReaderT Object (ExceptM m) a -> m ()
runExceptMWithObject = runExceptM . withJsonObject


-- | Same as 'ExceptT' but 'run' converts 'Either' into 'Value'.
newtype ExceptV m a = ExceptV { unExceptV :: ExceptT Text m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError Text
    , MonadThrow
    )

instance MonadTrans ExceptV where
  lift = ExceptV . lift

instance (MonadBase IO m) => MonadBase IO (ExceptV m) where
  liftBase = lift . liftBase

instance (MonadResource m) => MonadResource (ExceptV m) where 
  liftResourceT = lift . liftResourceT

instance (MonadHandler m) => MonadHandler (ExceptV m) where
  type HandlerSite (ExceptV m) = HandlerSite m
  liftHandlerT = lift . liftHandlerT


-- | Helper wrapper around 'Either'.
newtype Answer a = Answer (Either Text a)

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

-- | Run 'ExceptT'. Convert the result into 'Value'. Return it as json.
exceptToValue :: (ToJSON a, Monad m) => ExceptT Text m a -> m Value
exceptToValue f = do
  runExceptT f >>= returnJson . Answer

-- | Run 'ExceptV'. As a side effect, return json.
runExceptV :: (ToJSON a, Monad m) => ExceptV m a -> m Value
runExceptV = exceptToValue . unExceptV

-- | Run environment with json object and return json reply to user.
runExceptVWithObject
  :: (MonadHandler m, ToJSON a)
    => ReaderT Object (ExceptV m) a -> m Value
runExceptVWithObject = runExceptV . withJsonObject
