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
module Yesod.Util.Json where

import ClassyPrelude.Yesod hiding (Text, pack, Proxy, getEntity)

import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Aeson.Parser hiding (value, json)
import Data.Conduit.Attoparsec
import Data.Text (Text, pack)
import Data.Typeable

import Yesod.Util.Persist


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
  

-- | Evaluate 'ExceptJ' to this data type.
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


-- | Parse request body to json 'Value'.
parseJsonValue :: (MonadError Text m, MonadHandler m) => m Value
parseJsonValue = do
  eValue <- rawRequestBody $$ runCatchC (sinkParser value')
  case eValue of
    Left  e -> throwError $ pack $ show e
    Right v -> pure v

-- | Parse request body to json 'Value'; throw error on every case except 'Object'.
parseJsonObject :: (MonadError Text m, MonadHandler m) => m Object
parseJsonObject = do
  v <- parseJsonValue
  case v of
    Object obj -> pure obj
    _          -> throwError "json object expected"

-- | Evaluate 'JsonObjEnv' to 'ExceptJ'.
runJsonObjEnv :: (MonadHandler m) => JsonObjEnv m a -> ExceptJ m a
runJsonObjEnv f =
  parseJsonObject >>= runReaderT (getJsonObjEnv f)

-- | Evaluate 'JsonObjEnv' to json 'Value'.
withJsonObjEnv :: (MonadHandler m, ToJSON a) => JsonObjEnv m a -> m Value
withJsonObjEnv = runExceptJ . runJsonObjEnv


-- | Ask json object value by key.
askValue
  :: forall m a .
      (Monad m, FromJSON a, Typeable a) => Text -> JsonObjEnv m a
askValue key = do 
  obj <- ask
  case parse_ (obj .: key >>= parseJSON) of
    Success a -> pure a
    Error   _ -> throwError (errMsg key)
  where
    parse_ p = parse (const p) ()
    errMsg k = pack $
      "cannot parse '"
        ++ show (typeRep (Proxy :: Proxy a))
          ++ "' associated with key '"
            ++ show k
              ++ "'"

-- | Get 'Entity' from database using 'Key' from json object.
askEntity
  ::
  ( IsYesodPersistEntity master r
  , PersistStoreRead (YesodPersistBackend master)
  , Typeable r
  ) => Text -> JsonObjEnv (HandlerT master IO) (Entity r)
askEntity key = do
  askValue key >>= getEntity

-- | Get 'Entity' from database using 'Key' from json object;
-- make sure it matches provided predicate.
askEntityWhich
  ::
  ( IsYesodPersistEntity master r
  , PersistStoreRead (YesodPersistBackend master)
  , Typeable r
  ) => Predicate r -> Text -> JsonObjEnv (HandlerT master IO) (Entity r)
askEntityWhich p key = do
  askValue key >>= getEntityWhich p
  

-- | Wrapper for 'parseJsonBody'.
getJson
  :: forall m a .
       (MonadError Text m, MonadHandler m, FromJSON a, Typeable a) => m a
getJson = do
  json <- parseJsonBody
  case json of
    Success body -> pure body
    Error   _    -> throwError . pack $
                      "invalid json for " ++
                         show (typeRep (Proxy :: Proxy a))
