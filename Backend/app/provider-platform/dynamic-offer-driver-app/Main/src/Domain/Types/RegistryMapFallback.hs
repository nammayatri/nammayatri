{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.RegistryMapFallback where

import qualified "dashboard-helper-api" Dashboard.Common as Common
import Kernel.Prelude
import Kernel.Types.App ()

data RegistryMapFallback = RegistryMapFallback
  { subscriberId :: Text,
    uniqueId :: Text,
    registryUrl :: BaseUrl,
    updatedAt :: UTCTime
  }
  deriving (Generic, Show, Eq, FromJSON, ToJSON)

data RegistryMapReq = RegistryMapReq
  { subscriberId :: Maybe Text,
    uniqueId :: Maybe Text,
    registryUrl :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

instance Common.HideSecrets RegistryMapReq where
  hideSecrets = identity
