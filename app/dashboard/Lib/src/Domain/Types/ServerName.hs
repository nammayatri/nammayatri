{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServerName where

import Data.Singletons.TH
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.Dhall

data ServerName = APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show, Read, FromJSON, ToJSON, ToSchema)

genSingletons [''ServerName]

derivePersistField "ServerName"
