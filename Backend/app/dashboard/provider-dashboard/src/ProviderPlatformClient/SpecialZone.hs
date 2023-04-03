{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}

module ProviderPlatformClient.SpecialZone
  ( callSpecialZone,
  )
where

import qualified "special-zone" API.Types as SZ
import Domain.Types.ServerName
import "special-zone" Domain.Types.SpecialZone
import qualified EulerHS.Types as Euler
import Kernel.External.Maps.Types
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Client
import "lib-dashboard" Tools.Metrics

data SpecialZoneAPIs = SpecialZoneAPIs
  { lookupSpecialZone :: LatLong -> LatLong -> Euler.EulerClient [SpecialZone],
    createSpecialZone :: SpecialZoneAPIEntity -> Euler.EulerClient APISuccess,
    updateSpecialZone :: SpecialZone -> Euler.EulerClient APISuccess,
    deleteSpecialZone :: Id SpecialZone -> Euler.EulerClient APISuccess
  }

mkSpecialZoneAPIs :: Text -> SpecialZoneAPIs
mkSpecialZoneAPIs token = do
  SpecialZoneAPIs {..}
  where
    lookupSpecialZone
      :<|> createSpecialZone
      :<|> updateSpecialZone
      :<|> deleteSpecialZone = Euler.client (Proxy :: Proxy SZ.SpecialZoneDashboardAPIs) token

callSpecialZone ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DataServer]],
    CallServerAPI SpecialZoneAPIs m r b c
  ) =>
  (SpecialZoneAPIs -> b) ->
  c
callSpecialZone = callServerAPI @_ @m @r SPECIAL_ZONE mkSpecialZoneAPIs "callSpecialZone"
