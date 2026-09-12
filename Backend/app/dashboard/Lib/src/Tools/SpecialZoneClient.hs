{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Client for the special-zone service (the SPECIAL_ZONE data server).
--
-- Bodies are 'Value' rather than special-zone's domain types on purpose. Those
-- types live in the special-zone /application/ package, and linking a whole
-- service into an application server to forward four requests would be the wrong
-- dependency. This route is a pass-through -- the dashboard never inspected the
-- payload, it decoded and re-encoded it -- so carrying it as JSON preserves the
-- wire contract exactly.
--
-- The trade: the request is no longer decoded here, so a malformed body is
-- rejected by special-zone itself rather than at this hop, and these four routes
-- lose their OpenAPI schemas.
module Tools.SpecialZoneClient
  ( callSpecialZone,
    SpecialZoneAPIs (..),
  )
where

import qualified Domain.Types.ServerName as DSN
import qualified EulerHS.Types as Euler
import Kernel.External.Maps.Types (LatLong)
import Kernel.Prelude
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common
import Servant
import Tools.Client
import Tools.Metrics

type RegionLookupAPI =
  "lookup"
    :> MandatoryQueryParam "minLatLng" LatLong
    :> MandatoryQueryParam "maxLatLng" LatLong
    :> Get '[JSON] [Value]

type CreateSpecialZoneAPI = "create" :> ReqBody '[JSON] Value :> Post '[JSON] APISuccess

type UpdateSpecialZoneAPI = "update" :> ReqBody '[JSON] Value :> Post '[JSON] APISuccess

type DeleteSpecialZoneAPI = "delete" :> MandatoryQueryParam "id" Text :> Delete '[JSON] APISuccess

-- Mirrors special-zone's SpecialZoneDashboardAPIs: a static token, then the
-- four routes under "specialZone".
type SpecialZoneDashboardAPIs =
  Header "token" Text
    :> "specialZone"
    :> ( RegionLookupAPI
           :<|> CreateSpecialZoneAPI
           :<|> UpdateSpecialZoneAPI
           :<|> DeleteSpecialZoneAPI
       )

data SpecialZoneAPIs = SpecialZoneAPIs
  { lookupSpecialZone :: LatLong -> LatLong -> Euler.EulerClient [Value],
    createSpecialZone :: Value -> Euler.EulerClient APISuccess,
    updateSpecialZone :: Value -> Euler.EulerClient APISuccess,
    deleteSpecialZone :: Text -> Euler.EulerClient APISuccess
  }

mkSpecialZoneAPIs :: Text -> SpecialZoneAPIs
mkSpecialZoneAPIs token = do
  SpecialZoneAPIs {..}
  where
    lookupSpecialZone
      :<|> createSpecialZone
      :<|> updateSpecialZone
      :<|> deleteSpecialZone = Euler.client (Proxy :: Proxy SpecialZoneDashboardAPIs) (Just token)

callSpecialZone ::
  forall m r b c.
  ( CoreMetrics m,
    HasFlowEnv m r '["dataServers" ::: [DSN.DataServer]],
    CallServerAPI SpecialZoneAPIs m r b c
  ) =>
  (SpecialZoneAPIs -> b) ->
  c
callSpecialZone = callServerAPI @_ @m @r DSN.SPECIAL_ZONE mkSpecialZoneAPIs "callSpecialZone"
