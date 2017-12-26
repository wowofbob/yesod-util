{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ConstraintKinds     #-}
module Yesod.Util.Persist where

import ClassyPrelude.Yesod hiding (Proxy, Text, pack, getEntity)
import Control.Monad.Except
import Data.Text (Text, pack)
import Data.Typeable
import Yesod.Auth


-- | Shortcut constraint for something being a persist entity.
type IsPersistEntity r b =
  ( PersistEntity r 
  , PersistEntityBackend r ~ BaseBackend b
  )

-- | Shortcut constraint for something being a persist entity in yesod.
type IsYesodPersistEntity master r =
  ( YesodPersist master
  , IsPersistEntity r (YesodPersistBackend master)
  )


-- | Wrapper for 'get': throw error on 'Nothing'.
getEntity
  :: forall master r m .
  ( IsYesodPersistEntity master r
  , PersistStoreRead (YesodPersistBackend master)
  , Typeable r
  , MonadTrans m, MonadError Text (m (HandlerT master IO))
  ) => Key r -> m (HandlerT master IO) (Entity r)
getEntity k = do
  mr <- lift . runDB $ get k
  case mr of
    Just r  -> pure (Entity k r) 
    Nothing -> throwError . pack $
                 "unknown" ++ (show $ typeRep (Proxy :: Proxy r))


-- | Data type with test function and it's description.
data Predicate a = Predicate
  { predRun      :: a -> Bool
  , predDescribe :: String
  }

-- | Same as 'getEntity', but throws error when 'Predicate' doesn't hold.
getEntityWhich
  :: forall master r m .
  ( IsYesodPersistEntity master r
  , PersistStoreRead (YesodPersistBackend master)
  , Typeable r
  , MonadTrans m, MonadError Text (m (HandlerT master IO))
  ) => Predicate r -> Key r -> m (HandlerT master IO) (Entity r)
getEntityWhich p k = do
  e <- getEntity k
  when (not (predRun p (entityVal e))) $
    throwError . pack $
      (show $ typeRep (Proxy :: Proxy r))
        ++ "doesn't match predicate: "
          ++ (predDescribe p)
  pure e


-- | Wrapper for 'maybeAuthId'.
getUserId
  ::
  ( YesodAuth master
  , MonadTrans m, MonadError Text (m (HandlerT master IO))
  ) => m (HandlerT master IO) (AuthId master)
getUserId = do
  muid <- lift maybeAuthId
  case muid of
    Nothing  -> throwError "authentication is required"
    Just uid -> pure uid
