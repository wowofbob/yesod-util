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
{-# LANGUAGE FunctionalDependencies     #-}
module Yesod.Except.Wrappers
( withObjEnv
, ExceptV(..)
, runExceptV
, ExceptM(..)
, runExceptM
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


-- | Constraints for handler wrapped into 'MonadError'.
type family EHC m f a where
  EHC ExceptM f a = (MonadHandler f)
  EHC ExceptV f a = (Monad f, ToJSON a)

-- | Remove 'MonadError' layer back to handler.
class ErrorHandler m r | m -> r where
  unliftError :: EHC m f a => m f a -> f r

instance ErrorHandler ExceptM () where
  unliftError = runExceptM

instance ErrorHandler ExceptV Value where
  unliftError = runExceptV
  
  
-- | Reader with json 'Object' (may be changed).
newtype ObjEnv m a =
  ObjEnv
    { unObjEnv :: ReaderT Object (ExceptT Text m) a }
    deriving
      ( Functor
      , Applicative
      , Monad
      , MonadIO
      , MonadError  Text
      , MonadReader Object
      , MonadThrow
      )

instance MonadTrans ObjEnv where
  lift = ObjEnv . lift . lift

instance (MonadBase IO m) => MonadBase IO (ObjEnv m) where
  liftBase = lift . liftBase

instance (MonadResource m) => MonadResource (ObjEnv m) where 
  liftResourceT = lift . liftResourceT

instance (MonadHandler m) => MonadHandler (ObjEnv m) where
  type HandlerSite (ObjEnv m) = HandlerSite m
  liftHandlerT = lift . liftHandlerT


-- | Use json 'Object' as an environment. Provide constructor of 'ErrorHandler'
-- in order to specify the way an error gets rendered.
withObjEnv
  :: (MonadHandler m, ErrorHandler w r, EHC w f a)
  => (ExceptT Text m b -> w f a) -> ObjEnv m b -> f r
withObjEnv e f = unliftError . e $ do
  parseJsonObject >>= runReaderT (unObjEnv f)
