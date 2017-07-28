{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Handler.Util where

import ClassyPrelude.Yesod hiding (Proxy)
import Control.Monad.Except
import Data.Aeson.Types
import Data.Typeable
import Yesod.Auth


-- * runDB lifted to ExceptT

tryRunDB
  :: YesodPersist site
  => YesodDB site a -> ExceptT e (HandlerT site IO) a
tryRunDB = lift . runDB


-- * Json handling.

withJsonBody
  :: forall m a b .
       (MonadHandler m, FromJSON a, Typeable a)
  => (a -> ExceptT Text m b)
  -> ExceptT Text m b
withJsonBody f = do
  json <- parseJsonBody
  case json of
    Success body -> f body
    Error   _    -> throwError . pack $
                      "invalid json object for " ++
                         show (typeRep (Proxy :: Proxy a))


-- * Entity handling.

withEntity
  :: forall a site b .
       ( PersistEntity a
       , YesodPersistBackend site ~ PersistEntityBackend a
       , PersistStore (YesodPersistBackend site)
       , YesodPersist site
       , Typeable a )
  => Key a
  -> (Entity a -> ExceptT Text (HandlerT site IO) b)
  -> ExceptT Text (HandlerT site IO) b
withEntity key f =
  do mrec <- lift . runDB . get $ key
     case mrec of
       Just rec -> f (Entity key rec)
       Nothing  -> throwError . pack $
                     "unknown " ++ (show $ typeRep (Proxy :: Proxy a))

withEntity'
  :: ( PersistEntity a
     , YesodPersistBackend site ~ PersistEntityBackend a
     , PersistStore (YesodPersistBackend site)
     , YesodPersist site
     , Typeable a )
  => (a -> Bool)
  -> Text
  -> Key a
  -> (Entity a -> ExceptT Text (HandlerT site IO) b)
  -> ExceptT Text (HandlerT site IO) b
withEntity' p errMsg k f =
  withEntity k $ \ e -> do
    when (not (p (entityVal e))) $ throwError errMsg 
    f e


-- * Auth handling.

withAuthId
  :: YesodAuth site
  => (AuthId site -> ExceptT Text (HandlerT site IO) a)
  -> ExceptT Text (HandlerT site IO) a
withAuthId f = do
  maid <- lift maybeAuthId
  case maid of
    Nothing  -> throwError "authentication is required"
    Just aid -> f aid

withAuthEntity
  :: YesodAuthPersist site
  => (AuthEntity site -> ExceptT Text (HandlerT site IO) a)
  -> ExceptT Text (HandlerT site IO) a
withAuthEntity f = do
  withAuthId $ \ aid -> do
    mae <- lift $ getAuthEntity aid
    case mae of
      Nothing -> throwError "authentication failure"
      Just ae -> f ae
