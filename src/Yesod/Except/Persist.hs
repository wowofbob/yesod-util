{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Yesod.Except.Persist
( module Control.Monad.Except
, module Data.Text
, IsPersistEntity
, IsYesodPersistEntity
, getEntity_
) where

import ClassyPrelude.Yesod hiding (Proxy, Text, pack)
import Control.Monad.Except
import Data.Text (Text, pack)
import Data.Typeable


-- | Shortcut constraint for something being a 'PersistEntity'.
type IsPersistEntity r b =
  ( PersistEntity r 
  , PersistEntityBackend r ~ BaseBackend b
  )

-- | Shortcut constraint for something being a 'PersistEntity' in 'YesodPersist'.
type IsYesodPersistEntity master r =
  ( YesodPersist master
  , IsPersistEntity r (YesodPersistBackend master)
  )

-- | Lift 'getEntity' to 'MonadError': throw error on 'Nothing'.
getEntity_
  :: forall master r m .
  ( IsYesodPersistEntity master r
  , PersistStoreRead (YesodPersistBackend master)
  , Typeable r
  , MonadTrans m
  , MonadError Text (m (HandlerT master IO))
  ) => Key r -> m (HandlerT master IO) (Entity r)
getEntity_ k = do
  mr <- lift . runDB $ get k
  case mr of
    Just r  -> pure (Entity k r) 
    Nothing -> throwError . pack $
                 "unknown '"
                   ++ show (typeRep (Proxy :: Proxy r))
                     ++ "'"
