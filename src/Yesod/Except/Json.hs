{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Yesod.Except.Json
( module Control.Monad.Except
, module Data.Text
, parseJsonValue
, parseJsonObject
, parseJsonBody_
, askValue
, askEntity
, withJsonObject
, HasObject
) where

import ClassyPrelude.Yesod hiding (Text, pack, Proxy, parseJsonBody_)

import Control.Monad.Except
import Data.Aeson hiding (json)
import Data.Aeson.Types (parse)
import Data.Aeson.Parser hiding (value, json)
import Data.Conduit.Attoparsec (sinkParser)
import Data.Text (Text, pack)
import Data.Typeable

import Yesod.Except.Persist


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

-- | Lift 'parseJsonBody' to 'MonadError'.
parseJsonBody_
  :: forall m a .
       (MonadError Text m, MonadHandler m, FromJSON a, Typeable a) => m a
parseJsonBody_ = do
  json <- parseJsonBody
  case json of
    Success body -> pure body
    Error   _    -> throwError . pack $
                      "invalid json for " ++
                         show (typeRep (Proxy :: Proxy a))

-- | Environment with json object.
class HasObject r where
  getObject :: r -> Object

-- | Object is an instance of HasObject.
instance HasObject Object where
  getObject = id

-- | Retrieve json object field by key.
askValue
  :: forall m r a .
       ( MonadError Text m
       , MonadReader r m, HasObject r
       , FromJSON a
       , Typeable a
       ) => Text -> m a
askValue key = do
  obj <- getObject <$> ask
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
  , MonadTrans m
  , MonadError Text (m (HandlerT master IO))
  , MonadReader r (m (HandlerT master IO)), HasObject r
  ) => Text -> m (HandlerT master IO) (Entity r)
askEntity key = do
  askValue key >>= getEntity_

-- | Get json object from request body and use it as environment.
withJsonObject
  :: (MonadHandler m, MonadError Text m) => ReaderT Object m b -> m b
withJsonObject f = do
  parseJsonObject >>= runReaderT f
