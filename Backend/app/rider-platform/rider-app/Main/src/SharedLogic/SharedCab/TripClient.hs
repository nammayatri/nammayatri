{-# LANGUAGE TypeApplications #-}

-- | rider-app -> driver-app for a shared cab's run (vehicle_trip), which driver-app owns: the plate's ACTIVE trip, to rebuild a session
-- lost from Redis, and the end of a run rider-app closes itself.
module SharedLogic.SharedCab.TripClient
  ( TripClientFlow,
    DriverTrip (..),
    activeTrip,
    expireTrip,
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant as DM
import EulerHS.Types (EulerClient, client)
import Kernel.External.Slack.Types (Error)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Types.Error (MerchantError (..))
import Kernel.Types.Id
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQM

type TripClientFlow m r =
  ( CacheFlow m r,
    EsqDBFlow m r,
    MonadFlow m,
    CoreMetrics m,
    HasRequestId r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  )

-- | agencyId is driver-app's integrated_bpp_config.agency_key, which names rider-app's config of the same feed.
data DriverTrip = DriverTrip
  { id :: Text,
    driverId :: Text,
    vehicleNumber :: Text,
    agencyId :: Text,
    routeCode :: Text,
    capacity :: Maybe Int,
    walkupCount :: Int,
    startedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

type TripAPI api = "internal" :> "sharedCab" :> Capture "merchantId" Text :> "trip" :> api

type ActiveTripAPI = TripAPI ("active" :> MandatoryQueryParam "vehicleNumber" Text :> Header "token" Text :> Get '[JSON] (Maybe DriverTrip))

type ExpireTripAPI = TripAPI (Capture "tripId" Text :> "expire" :> QueryParam "earnings" HighPrecMoney :> Header "token" Text :> Post '[JSON] APISuccess)

activeTrip :: TripClientFlow m r => Id DM.Merchant -> Text -> m (Maybe DriverTrip)
activeTrip merchantId plate = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  callDriver merchant "SharedCabActiveTrip" (client (Proxy @ActiveTripAPI) merchant.driverOfferMerchantId plate (Just merchant.driverOfferApiKey)) (Proxy @ActiveTripAPI)

-- | rider-app ends a run itself only when the cab went silent.
expireTrip :: TripClientFlow m r => Id DM.Merchant -> Text -> Maybe HighPrecMoney -> m ()
expireTrip merchantId tripId earnings = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  void $ callDriver merchant "SharedCabExpireTrip" (client (Proxy @ExpireTripAPI) merchant.driverOfferMerchantId tripId earnings (Just merchant.driverOfferApiKey)) (Proxy @ExpireTripAPI)

callDriver :: (TripClientFlow m r, SanitizedUrl api, ToJSON a) => DM.Merchant -> Text -> EulerClient a -> Proxy api -> m a
callDriver merchant desc eulerClient api = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @Error) Nothing (Just "BPP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) merchant.driverOfferBaseUrl eulerClient desc api
