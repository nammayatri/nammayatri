{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServerName where

import Beckn.Prelude
import Beckn.Storage.Esqueleto
import Beckn.Utils.Dhall
import Data.Singletons.TH

-- Each merchant on BAP side is considered like separate BAP
data ServerName = APP_BACKEND_YATRI | APP_BACKEND_ARDU | BECKN_TRANSPORT | DRIVER_OFFER_BPP
  deriving (Generic, FromDhall, Eq, Show, Read, FromJSON, ToJSON, ToSchema)

genSingletons [''ServerName]

derivePersistField "ServerName"
