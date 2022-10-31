{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServerName where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Utils.Dhall
import Data.Singletons.TH

data ServerName = APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show, Read, FromJSON, ToJSON, ToSchema)

genSingletons [''ServerName]

derivePersistField "ServerName"
