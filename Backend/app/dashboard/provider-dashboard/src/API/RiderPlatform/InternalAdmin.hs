{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.RiderPlatform.InternalAdmin
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Booking as BTypes
import qualified "dashboard-helper-api" API.Types.RiderPlatform.Management.Ride as RTypes
import qualified Domain.Action.RiderPlatform.Management.Booking as DBooking
import qualified Domain.Action.RiderPlatform.Management.Ride as DRide
import qualified "lib-dashboard" Domain.Types.Merchant as DM
import "lib-dashboard" Domain.Types.ServerName (ServerName (..))
import "lib-dashboard" Environment
import Kernel.Prelude
import qualified Kernel.Types.Beckn.City as City
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import Storage.Beam.CommonInstances ()
import qualified "lib-dashboard" Storage.Queries.Merchant as QMerchant
import "lib-dashboard" Tools.Auth.Merchant

-- | Internal cluster-only administration endpoints for the rider platform,
-- mirroring @API.ProviderPlatform.DynamicOfferDriver.InternalAdmin@.
--
-- There is intentionally NO dashboard person token / RBAC guard on these
-- routes. The only credential is the static @internalAuthAPIKey@ service
-- token passed in the @api-key@ header, and it authorizes the operation for
-- EVERY merchant on the instance; the target merchant comes from the
-- @merchantId@/@city@ path segments. The operation bodies themselves are the
-- exact same implementations the RBAC-guarded routes use
-- (@Domain.Action.RiderPlatform.Management.*@).
--
-- Mounted under @\/bap\/internal\/admin\/...@. NEVER expose this through a
-- public ingress — it is meant for cluster-internal automation
-- (stuck-booking cron jobs, ops tooling) reached over the dashboard's
-- in-cluster service DNS.
type API =
  "internal"
    :> "admin"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" City.City
    :> Header "api-key" Text
    :> ( RideInternalAPI
           :<|> BookingInternalAPI
       )

-- | Same requestor tag as the provider-platform internal admin API.
defaultRequestorId :: Text
defaultRequestorId = "INTERNAL_ADMIN_API"

type RideInternalAPI =
  "ride"
    :> "sync"
    :> ReqBody '[JSON] RTypes.MultipleRideSyncReq
    :> Post '[JSON] RTypes.MultipleRideSyncResp

type BookingInternalAPI =
  "booking"
    :> "sync"
    :> ReqBody '[JSON] BTypes.MultipleBookingSyncReq
    :> Post '[JSON] BTypes.MultipleBookingSyncResp

verifyApiKey :: Maybe Text -> Flow ()
verifyApiKey mbApiKey = do
  internalAuthAPIKey <- asks (.internalAuthAPIKey)
  unless (Just internalAuthAPIKey == mbApiKey) $
    throwError (InvalidRequest "Invalid API key")

-- | Any merchant that exists and serves this dashboard's BAP platform is
-- allowed — there is deliberately no per-merchant allowlist: the static token
-- is the only gate and it applies uniformly to all merchants.
checkMerchant :: ShortId DM.Merchant -> City.City -> Flow (CheckedShortId DM.Merchant, Id DM.Merchant)
checkMerchant merchantShortId opCity = do
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  unless (APP_BACKEND_MANAGEMENT `elem` merchant.serverNames) $
    throwError AccessDenied
  unless (opCity `elem` merchant.supportedOperatingCities) $
    throwError AccessDenied
  pure (skipMerchantCityAccessCheck merchantShortId, merchant.id)

handler :: FlowServer API
handler merchantShortId opCity apiKey =
  postRideSyncInternal merchantShortId opCity apiKey
    :<|> postBookingSyncInternal merchantShortId opCity apiKey

postRideSyncInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> RTypes.MultipleRideSyncReq -> FlowHandler RTypes.MultipleRideSyncResp
postRideSyncInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DRide.postRideSyncMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req

postBookingSyncInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> BTypes.MultipleBookingSyncReq -> FlowHandler BTypes.MultipleBookingSyncResp
postBookingSyncInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DBooking.postBookingSyncMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req
