{-# LANGUAGE TypeApplications #-}

-- | driver-app -> rider-app client for the shared-cab internal API (/internal/sharedCab/*). Every call maps the rider app's
-- APIError envelope onto BapProxyError, so the driver app surfaces the rider app's errorCode and HTTP status unchanged.
module SharedLogic.SharedCab.RiderClient
  ( BAPRouteOperationReq (..),
    BAPSessionOperationReq (..),
    BAPBookingOperationReq (..),
    BAPDemandReq (..),
    getSharedCabRouteData,
    postSharedCabRoutesDemand,
    NearbyStandDemand (..),
    getSharedCabNearbyDemand,
    postRouteOperation,
    postSessionOperation,
    postBookingOperation,
    RiderCallNumberResp (..),
    getRiderCallNumber,
    BAPMessageReq (..),
    postRiderMessage,
    riderClientTimeoutMs,
    prepareRiderClientHttpManager,
  )
where

import API.Types.UI.SharedCab
import qualified Data.HashMap.Strict as HM
import Domain.Types.Extra.SharedCab ()
import Domain.Types.SharedCabSession
import EulerHS.Types (EulerClient, client)
import qualified EulerHS.Types as ET
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.APISuccess (APISuccess)
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import qualified Kernel.Utils.Servant.Client as EC
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Servant hiding (throwError)
import Tools.Error (BapProxyError (..))

type BAPFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  )

-- | Contract: `mode` is passed as null per the rider-app endpoint convention.
data BAPRouteOperationReq = BAPRouteOperationReq
  { operation :: SharedCabRouteOperation,
    driverId :: Text,
    vehicleNumber :: Text,
    -- the SHARED_CAB config's agency_key (GTFS agency gtfsId like "<feed>:SHARED_CAB"), the one identity both
    -- integrated_bpp_config rows share; rider-app looks its own row up by it
    agencyId :: Maybe Text,
    capacity :: Maybe Int,
    routeCode :: Maybe Text,
    walkupCount :: Maybe Int,
    mode :: Maybe SelectRouteMode,
    driverName :: Maybe Text,
    vehicleModel :: Maybe Text,
    atLastStop :: Maybe Bool,
    forceEnd :: Maybe Bool,
    vehicleTripId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPSessionOperationReq = BAPSessionOperationReq
  { operation :: SharedCabSessionOperation,
    driverId :: Text,
    vehicleNumber :: Text,
    version :: Maybe Int,
    walkupCount :: Maybe Int,
    checkInId :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPBookingOperationReq = BAPBookingOperationReq
  { operation :: SharedCabBookingOperation,
    driverId :: Text,
    vehicleNumber :: Text,
    reason :: Maybe Text,
    otp :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPDemandReq = BAPDemandReq
  { agencyId :: Text,
    routeCodes :: [Text]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type InternalAPI api = "internal" :> "sharedCab" :> api

type TokenHeader = Header "token" Text

type RouteDataAPI =
  InternalAPI
    ( "routes"
        :> "data"
        :> QueryParam' '[Required, Strict] "agencyId" Text
        :> TokenHeader
        :> Get '[JSON] SharedCabRouteDataResp
    )

type DemandAPI = InternalAPI ("routes" :> "demand" :> TokenHeader :> ReqBody '[JSON] BAPDemandReq :> Post '[JSON] SharedCabDemandResp)

data NearbyStandDemand = NearbyStandDemand
  { stopCode :: Text,
    lat :: Double,
    lon :: Double,
    searching :: Int
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

type NearbyDemandAPI =
  InternalAPI
    ( "nearbyDemand"
        :> QueryParam' '[Required, Strict] "agencyId" Text
        :> QueryParam' '[Required, Strict] "lat" Double
        :> QueryParam' '[Required, Strict] "lon" Double
        :> TokenHeader
        :> Get '[JSON] [NearbyStandDemand]
    )

type RouteOperationAPI = InternalAPI ("route" :> "operations" :> TokenHeader :> ReqBody '[JSON] BAPRouteOperationReq :> Post '[JSON] SelectRouteResp)

type SessionOperationAPI = InternalAPI ("session" :> "operations" :> TokenHeader :> ReqBody '[JSON] BAPSessionOperationReq :> Post '[JSON] SharedCabSession)

type BookingOperationAPI = InternalAPI ("booking" :> Capture "bookingId" Text :> "operations" :> TokenHeader :> ReqBody '[JSON] BAPBookingOperationReq :> Post '[JSON] SharedCabSession)

newtype RiderCallNumberResp = RiderCallNumberResp {riderNumber :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON, FromJSON)

type CallNumberAPI =
  InternalAPI
    ( "booking"
        :> Capture "bookingId" Text
        :> "callNumber"
        :> QueryParam' '[Required, Strict] "driverId" Text
        :> QueryParam' '[Required, Strict] "vehicleNumber" Text
        :> TokenHeader
        :> Get '[JSON] RiderCallNumberResp
    )

data BAPMessageReq = BAPMessageReq
  { driverId :: Text,
    vehicleNumber :: Text,
    message :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToJSON)

type MessageAPI = InternalAPI ("booking" :> Capture "bookingId" Text :> "message" :> TokenHeader :> ReqBody '[JSON] BAPMessageReq :> Post '[JSON] APISuccess)

callRider :: (BAPFlow m r, SanitizedUrl api, ToJSON a) => BaseUrl -> Text -> EulerClient a -> Proxy api -> m a
callRider internalUrl desc eulerClient api = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @BapProxyError) (Just (ET.ManagerSelector riderClientManagerKey)) (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl eulerClient desc api

-- | Every driver action waits on one call to rider-app: a slow rider-app fails it in 5 s instead of the default client timeout.
riderClientTimeoutMs :: Int
riderClientTimeoutMs = 5000

riderClientManagerKey :: Text
riderClientManagerKey = "shared-cab-rider-client"

prepareRiderClientHttpManager :: HM.HashMap Text Http.ManagerSettings
prepareRiderClientHttpManager =
  HM.singleton riderClientManagerKey $ Http.tlsManagerSettings {Http.managerResponseTimeout = Http.responseTimeoutMicro (riderClientTimeoutMs * 1000)}

getSharedCabRouteData :: BAPFlow m r => Text -> BaseUrl -> Text -> m SharedCabRouteDataResp
getSharedCabRouteData apiKey internalUrl agencyId = do
  callRider internalUrl "GetSharedCabRouteData" (client (Proxy @RouteDataAPI) agencyId (Just apiKey)) (Proxy @RouteDataAPI)

postSharedCabRoutesDemand :: BAPFlow m r => Text -> BaseUrl -> BAPDemandReq -> m SharedCabDemandResp
postSharedCabRoutesDemand apiKey internalUrl req = do
  callRider internalUrl "PostSharedCabRoutesDemand" (client (Proxy @DemandAPI) (Just apiKey) req) (Proxy @DemandAPI)

getSharedCabNearbyDemand :: BAPFlow m r => Text -> BaseUrl -> Text -> Double -> Double -> m [NearbyStandDemand]
getSharedCabNearbyDemand apiKey internalUrl agencyId latitude longitude =
  callRider internalUrl "GetSharedCabNearbyDemand" (client (Proxy @NearbyDemandAPI) agencyId latitude longitude (Just apiKey)) (Proxy @NearbyDemandAPI)

postRouteOperation :: BAPFlow m r => Text -> BaseUrl -> BAPRouteOperationReq -> m SelectRouteResp
postRouteOperation apiKey internalUrl req = do
  callRider internalUrl "SharedCabRouteOperation" (client (Proxy @RouteOperationAPI) (Just apiKey) req) (Proxy @RouteOperationAPI)

postSessionOperation :: BAPFlow m r => Text -> BaseUrl -> BAPSessionOperationReq -> m SharedCabSession
postSessionOperation apiKey internalUrl req = do
  callRider internalUrl "SharedCabSessionOperation" (client (Proxy @SessionOperationAPI) (Just apiKey) req) (Proxy @SessionOperationAPI)

postBookingOperation :: BAPFlow m r => Text -> BaseUrl -> Text -> BAPBookingOperationReq -> m SharedCabSession
postBookingOperation apiKey internalUrl bookingId req = do
  logInfo $ "CallSharedCabBooking: " <> show req.operation <> " booking " <> bookingId <> " by driver " <> req.driverId
  callRider internalUrl "SharedCabBookingOperation" (client (Proxy @BookingOperationAPI) bookingId (Just apiKey) req) (Proxy @BookingOperationAPI)

-- | The rider's phone for a driver-initiated masked call; rider-app answers only for the driver whose live cab holds the booking.
getRiderCallNumber :: BAPFlow m r => Text -> BaseUrl -> Text -> Text -> Text -> m RiderCallNumberResp
getRiderCallNumber apiKey internalUrl bookingId driverId vehicleNumber =
  callRider internalUrl "GetSharedCabRiderCallNumber" (client (Proxy @CallNumberAPI) bookingId driverId vehicleNumber (Just apiKey)) (Proxy @CallNumberAPI)

-- | The driver's chat message for a rider; rider-app checks the booking is on this driver's live cab and sends the push.
postRiderMessage :: BAPFlow m r => Text -> BaseUrl -> Text -> BAPMessageReq -> m APISuccess
postRiderMessage apiKey internalUrl bookingId req =
  callRider internalUrl "PostSharedCabRiderMessage" (client (Proxy @MessageAPI) bookingId (Just apiKey) req) (Proxy @MessageAPI)
