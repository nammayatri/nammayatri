module API.ProviderPlatform.DynamicOfferDriver.InternalAdmin
  ( API,
    handler,
  )
where

import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Booking as BTypes
import qualified "dashboard-helper-api" API.Types.ProviderPlatform.Management.Ride as RTypes
import qualified Domain.Action.ProviderPlatform.Management.Booking as DBooking
import qualified Domain.Action.ProviderPlatform.Management.Ride as DRide
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

-- | Internal cluster-only administration endpoints for the provider platform.
--
-- There is intentionally NO dashboard person token / RBAC guard on these
-- routes. The only credential is the static @internalAuthAPIKey@ service
-- token passed in the @api-key@ header, and it authorizes the operation for
-- EVERY merchant on the instance; the target merchant comes from the
-- @merchantId@/@city@ path segments. The operation bodies themselves are the
-- exact same implementations the RBAC-guarded routes use
-- (@Domain.Action.ProviderPlatform.Management.*@).
--
-- Mounted under @\/bpp\/driver-offer\/internal\/admin\/...@, next to the other
-- internal services. NEVER expose this through a public ingress — it is meant
-- for cluster-internal automation (stuck-booking cron jobs, ops tooling)
-- reached over the dashboard's in-cluster service DNS.
type API =
  "internal"
    :> "admin"
    :> Capture "merchantId" (ShortId DM.Merchant)
    :> Capture "city" City.City
    :> Header "api-key" Text
    :> ( RideInternalAPI
           :<|> BookingInternalAPI
       )

-- | Tag recorded as the requestor of internal calls in the dashboard
-- transaction table and forwarded to the provider app, so internal ops stay
-- attributable in logs/audit (no dashboard person exists for them).
defaultRequestorId :: Text
defaultRequestorId = "INTERNAL_ADMIN_API"

type RideInternalAPI =
  "ride"
    :> ( RideEndInternalAPI
           :<|> RideCancelInternalAPI
           :<|> RideSyncInternalAPI
       )

type RideEndInternalAPI =
  "end"
    :> ReqBody '[JSON] RTypes.MultipleRideEndReq
    :> Post '[JSON] RTypes.MultipleRideEndResp

type RideCancelInternalAPI =
  "cancel"
    :> ReqBody '[JSON] RTypes.MultipleRideCancelReq
    :> Post '[JSON] RTypes.MultipleRideCancelResp

type RideSyncInternalAPI =
  "sync"
    :> ReqBody '[JSON] RTypes.MultipleRideSyncReq
    :> Post '[JSON] RTypes.MultipleRideSyncRes

type BookingInternalAPI =
  "booking"
    :> ( BookingSyncInternalAPI
           :<|> BookingCancelAllStuckInternalAPI
       )

type BookingSyncInternalAPI =
  "sync"
    :> ReqBody '[JSON] BTypes.MultipleBookingSyncReq
    :> Post '[JSON] BTypes.MultipleBookingSyncResp

type BookingCancelAllStuckInternalAPI =
  "cancel"
    :> "allStuck"
    :> ReqBody '[JSON] BTypes.StuckBookingsCancelReq
    :> Post '[JSON] BTypes.StuckBookingsCancelRes

verifyApiKey :: Maybe Text -> Flow ()
verifyApiKey mbApiKey = do
  internalAuthAPIKey <- asks (.internalAuthAPIKey)
  unless (Just internalAuthAPIKey == mbApiKey) $
    throwError (InvalidRequest "Invalid API key")

-- | Any merchant that exists and serves this dashboard's BPP platform is
-- allowed — there is deliberately no per-merchant allowlist: the static token
-- is the only gate and it applies uniformly to all merchants.
checkMerchant :: ShortId DM.Merchant -> City.City -> Flow (CheckedShortId DM.Merchant, Id DM.Merchant)
checkMerchant merchantShortId opCity = do
  merchant <- QMerchant.findByShortId merchantShortId >>= fromMaybeM (MerchantNotFound merchantShortId.getShortId)
  unless (DRIVER_OFFER_BPP_MANAGEMENT `elem` merchant.serverNames) $
    throwError AccessDenied
  unless (opCity `elem` merchant.supportedOperatingCities) $
    throwError AccessDenied
  pure (skipMerchantCityAccessCheck merchantShortId, merchant.id)

handler :: FlowServer API
handler merchantShortId opCity apiKey =
  rideHandler merchantShortId opCity apiKey
    :<|> bookingHandler merchantShortId opCity apiKey

rideHandler :: ShortId DM.Merchant -> City.City -> Maybe Text -> FlowServer RideInternalAPI
rideHandler merchantShortId opCity apiKey =
  postRideEndInternal merchantShortId opCity apiKey
    :<|> postRideCancelInternal merchantShortId opCity apiKey
    :<|> postRideSyncInternal merchantShortId opCity apiKey

bookingHandler :: ShortId DM.Merchant -> City.City -> Maybe Text -> FlowServer BookingInternalAPI
bookingHandler merchantShortId opCity apiKey =
  postBookingSyncInternal merchantShortId opCity apiKey
    :<|> postBookingCancelAllStuckInternal merchantShortId opCity apiKey

postRideEndInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> RTypes.MultipleRideEndReq -> FlowHandler RTypes.MultipleRideEndResp
postRideEndInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DRide.postRideEndMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req

postRideCancelInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> RTypes.MultipleRideCancelReq -> FlowHandler RTypes.MultipleRideCancelResp
postRideCancelInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DRide.postRideCancelMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req

postRideSyncInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> RTypes.MultipleRideSyncReq -> FlowHandler RTypes.MultipleRideSyncRes
postRideSyncInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DRide.postRideSyncMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req

postBookingSyncInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> BTypes.MultipleBookingSyncReq -> FlowHandler BTypes.MultipleBookingSyncResp
postBookingSyncInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DBooking.postBookingSyncMultipleInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req

postBookingCancelAllStuckInternal :: ShortId DM.Merchant -> City.City -> Maybe Text -> BTypes.StuckBookingsCancelReq -> FlowHandler BTypes.StuckBookingsCancelRes
postBookingCancelAllStuckInternal merchantShortId opCity apiKey req = withFlowHandlerAPI' $ do
  verifyApiKey apiKey
  (checkedMerchantId, merchantId) <- checkMerchant merchantShortId opCity
  DBooking.postBookingCancelAllStuckInternal checkedMerchantId opCity (Just defaultRequestorId) (Just merchantId) req
