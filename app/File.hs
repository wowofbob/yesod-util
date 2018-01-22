{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module File where

import ClassyPrelude.Yesod

import File.Data

instance
  ( YesodFile master ) =>
    YesodSubDispatch File (HandlerT master IO) where
      yesodSubDispatch = $(mkYesodSubDispatch resourcesFile)
