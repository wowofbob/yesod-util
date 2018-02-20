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


-- | Wrapper around 'ExceptT' with a side effect of setting message in user
-- session on error.
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


-- | Special runner for 'ExceptT':
-- do nothing on 'Right' and ingnore result;
-- call 'setMessage' on the value of 'Left'. 
exceptToMessage :: (ToMarkup t, MonadHandler m) => ExceptT t m a -> m ()
exceptToMessage f = do
  eres <- runExceptT f
  case eres of
    Left  msg -> setMessage . toHtml $ msg
    Right _   -> pure ()

-- | Runner for 'ExceptM': set message in user session on error.
runExceptM :: (MonadHandler m) => ExceptM m a -> m ()
runExceptM = exceptToMessage . unExceptM

-- | Run environment with json object and set message in user session on error.
runExceptMWithObject
  :: (MonadHandler m)
    => ReaderT Object (ExceptM m) a -> m ()
runExceptMWithObject = runExceptM . withJsonObject


-- | Wrapper around 'ExceptT' with a side effect of returning json answer back
-- to user application.
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


-- | Helper wrapper around 'Either' for running 'ExceptV'.
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

-- | Specical runner for 'ExceptT': convert the result to 'Value' using
-- 'Answer' for serialization.
exceptToValue :: (ToJSON a, Monad m) => ExceptT Text m a -> m Value
exceptToValue f = do
  runExceptT f >>= returnJson . Answer

-- | Runner for 'ExceptV': return json answer back to user application.
runExceptV :: (ToJSON a, Monad m) => ExceptV m a -> m Value
runExceptV = exceptToValue . unExceptV

-- | Run environment with json object and return json answer back to user
-- application.
runExceptVWithObject
  :: (MonadHandler m, ToJSON a)
    => ReaderT Object (ExceptV m) a -> m Value
runExceptVWithObject = runExceptV . withJsonObject
