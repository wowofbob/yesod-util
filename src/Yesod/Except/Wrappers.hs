{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ExistentialQuantification  #-}
module Yesod.Except.Wrappers
( ExceptJ(..)
, runExceptJ
, JsonObjEnv(..)
, runJsonObjEnv
, withJsonObjEnv
) where

import ClassyPrelude.Yesod hiding (Text, pack)

import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Text (Text, pack)
import Text.Blaze

import Yesod.Except.Json


-- | Like 'ExceptT' but evaluates to json 'Value'.
-- If m is a 'MonadHandler', then 'ExceptJ' is a MonadHandler.
-- So 'HandlerT's code may be run inside 'ExceptJ'.
newtype ExceptJ m a =
  ExceptJ { getExceptJ :: ExceptT Text m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError Text
    , MonadThrow
    )

instance MonadTrans ExceptJ where
  lift = ExceptJ . lift

instance (MonadBase IO m) => MonadBase IO (ExceptJ m) where
  liftBase = lift . liftBase

instance (MonadResource m) => MonadResource (ExceptJ m) where 
  liftResourceT = lift . liftResourceT

instance (MonadHandler m) => MonadHandler (ExceptJ m) where
  type HandlerSite (ExceptJ m) = HandlerSite m
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

-- | Runner for 'ExceptJ'.
runExceptJ :: (ToJSON a, Monad m) => ExceptJ m a -> m Value
runExceptJ f =
  runExceptT (getExceptJ f) >>= returnJson . Answer


-- | Environment with json object inside.
-- May be evaluated to 'ExceptJ' or to json 'Value'.
-- If m is 'MonadHandler', then 'JsonObjEnv' is 'MonadHandler'.
-- So 'HandlerT's code may be run inside 'JsonObjEnv'.
newtype JsonObjEnv m a =
  JsonObjEnv
    { getJsonObjEnv :: ReaderT Object (ExceptJ m) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadError  Text
      , MonadReader Object
      , MonadThrow
      )

instance MonadTrans JsonObjEnv where
  lift = JsonObjEnv . lift . lift

instance (MonadBase IO m) => MonadBase IO (JsonObjEnv m) where
  liftBase = lift . liftBase

instance (MonadResource m) => MonadResource (JsonObjEnv m) where 
  liftResourceT = lift . liftResourceT

instance (MonadHandler m) => MonadHandler (JsonObjEnv m) where
  type HandlerSite (JsonObjEnv m) = HandlerSite m
  liftHandlerT = lift . liftHandlerT


-- | Evaluate 'JsonObjEnv' to 'ExceptJ'.
runJsonObjEnv :: (MonadHandler m) => JsonObjEnv m a -> ExceptJ m a
runJsonObjEnv f =
  parseJsonObject >>= runReaderT (getJsonObjEnv f)

-- | Evaluate 'JsonObjEnv' to json 'Value'.
withJsonObjEnv :: (MonadHandler m, ToJSON a) => JsonObjEnv m a -> m Value
withJsonObjEnv = runExceptJ . runJsonObjEnv


-- | Run 'ExceptT'. On error, call 'setMessage'. Otherwise, ignore result.
exceptToMessage :: (ToMarkup t, MonadHandler m) => ExceptT t m a -> m ()
exceptToMessage f = do
  eres <- runExceptT f
  case eres of
    Left  msg -> setMessage . toHtml $ msg
    Right _   -> pure ()
