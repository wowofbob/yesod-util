{-# LANGUAGE NoImplicitPrelude   #-}
module Yesod.Except
( module Control.Monad.Except
, module Data.Text
, module Yesod.Except.Json
, module Yesod.Except.Persist
, module Yesod.Except.Wrappers
, (?>)
) where

import Control.Monad.Except
import Data.Text

import Yesod.Except.Json
import Yesod.Except.Persist
import Yesod.Except.Wrappers


-- | Replace error.
(?>) :: MonadError e m => m a -> e -> m a
action ?> msg = action `catchError` (\_ -> throwError msg)
