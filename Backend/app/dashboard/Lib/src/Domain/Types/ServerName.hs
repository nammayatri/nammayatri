{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE TemplateHaskell #-}

module Domain.Types.ServerName where

import Data.Singletons.TH
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Utils.Dhall

data DataServer = DataServer
  { name :: ServerName,
    url :: BaseUrl,
    token :: Text
  }
  deriving (Generic, FromDhall)

data ServerName = APP_BACKEND | APP_BACKEND_MANAGEMENT | DRIVER_OFFER_BPP | DRIVER_OFFER_BPP_MANAGEMENT | SPECIAL_ZONE
  deriving (Generic, FromDhall, Eq, Show, Read, FromJSON, ToJSON, ToSchema)

genSingletons [''ServerName]

derivePersistField "ServerName"
