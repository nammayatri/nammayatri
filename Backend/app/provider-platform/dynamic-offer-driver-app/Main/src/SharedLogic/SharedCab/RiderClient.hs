{-# LANGUAGE TypeApplications #-}

-- | driver-app -> rider-app client for the shared-cab internal API (/internal/sharedCab/*). Every call maps the rider app's
-- APIError envelope onto SharedCabBAPError, so the driver app surfaces the rider app's errorCode and HTTP status unchanged.
module SharedLogic.SharedCab.RiderClient
  ( BAPFlow,
    SessionAPI,
    BAPSelectRouteReq (..),
    BAPSeatsReq (..),
    BAPEndRouteReq (..),
    BAPResumeReq (..),
    BAPDriverReq (..),
    sessionClient,
    sessionProbeClient,
    getSharedCabRoutes,
    selectSharedCabRoute,
    getSharedCabSession,
    setSharedCabSeats,
    endSharedCabRoute,
    resumeSharedCab,
    getSharedCabTrips,
    postBookingAction,
    postCabFull,
  )
where

import API.Types.UI.SharedCab
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import EulerHS.Types (EulerClient, client)
import Kernel.Prelude
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics)
import Kernel.Types.Error (ExternalAPICallError (..))
import qualified Kernel.Types.Error.BaseError.HTTPError.CallAPIError as CallAPIError
import Kernel.Utils.Common hiding (Error)
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import qualified Kernel.Utils.Servant.Client as EC
import Servant hiding (throwError)
import Tools.Error (SharedCabBAPError (..))

type BAPFlow m r =
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  )

-- | Contract: `mode` is passed as null per the rider-app endpoint convention.
data BAPSelectRouteReq = BAPSelectRouteReq
  { mode :: Maybe SelectRouteMode,
    routeCode :: Text,
    walkupCount :: Int,
    driverId :: Text,
    vehicleNumber :: Text,
    -- the SHARED_CAB config's agency_key (GTFS agency gtfsId like "<feed>:SHARED_CAB"), the one identity both apps'
    -- integrated_bpp_config rows share; rider-app looks its own row up by it
    agencyId :: Text,
    serviceTierType :: Text,
    capacity :: Int,
    driverName :: Maybe Text,
    vehicleModel :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPSeatsReq = BAPSeatsReq
  { version :: Int,
    walkupCount :: Int,
    driverId :: Text,
    vehicleNumber :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPEndRouteReq = BAPEndRouteReq
  { next :: EndRouteNext,
    atLastStop :: Maybe Bool,
    force :: Maybe Bool,
    driverId :: Text,
    vehicleNumber :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPResumeReq = BAPResumeReq
  { driverId :: Text,
    vehicleNumber :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

data BAPDriverReq = BAPDriverReq
  { driverId :: Text,
    vehicleNumber :: Text,
    reason :: Maybe Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

type InternalAPI api = "internal" :> "sharedCab" :> api

type TokenHeader = Header "token" Text

type RoutesAPI =
  InternalAPI
    ( "routes"
        :> QueryParam' '[Required, Strict] "agencyId" Text
        :> QueryParam' '[Required, Strict] "lat" Double
        :> QueryParam' '[Required, Strict] "lon" Double
        :> TokenHeader
        :> Get '[JSON] SharedCabRoutesResp
    )

type SelectRouteAPI = InternalAPI ("route" :> "select" :> TokenHeader :> ReqBody '[JSON] BAPSelectRouteReq :> Post '[JSON] SelectRouteResp)

-- | Parameterised on the response so the reconciler can probe the same endpoint without decoding the session.
type SessionAPI res =
  InternalAPI
    ( "session"
        :> QueryParam' '[Required, Strict] "driverId" Text
        :> QueryParam' '[Required, Strict] "vehicleNumber" Text
        :> TokenHeader
        :> Get '[JSON] res
    )

type SeatsAPI = InternalAPI ("seats" :> TokenHeader :> ReqBody '[JSON] BAPSeatsReq :> Post '[JSON] SharedCabSession)

type EndRouteAPI = InternalAPI ("route" :> "end" :> TokenHeader :> ReqBody '[JSON] BAPEndRouteReq :> Post '[JSON] (Maybe SharedCabSession))

type ResumeAPI = InternalAPI ("resume" :> TokenHeader :> ReqBody '[JSON] BAPResumeReq :> Post '[JSON] SharedCabSession)

-- | `date` is mandatory per the rider-app contract: an ISO-8601 calendar day, defaulted to today in Domain.Action.UI.SharedCab.
type TripsAPI =
  InternalAPI
    ( "trips"
        :> QueryParam' '[Required, Strict] "driverId" Text
        :> QueryParam' '[Required, Strict] "date" Text
        :> TokenHeader
        :> Get '[JSON] SharedCabTripsResp
    )

type BookingActionAPI = InternalAPI ("booking" :> Capture "bookingId" Text :> Capture "action" Text :> TokenHeader :> ReqBody '[JSON] BAPDriverReq :> Post '[JSON] SharedCabSession)

type CabFullAPI = InternalAPI ("cabFull" :> TokenHeader :> ReqBody '[JSON] BAPDriverReq :> Post '[JSON] SharedCabSession)

sessionClient :: Text -> Text -> Maybe Text -> EulerClient (Maybe SharedCabSession)
sessionClient = client (Proxy @(SessionAPI (Maybe SharedCabSession)))

-- | The reconciler only asks whether a session exists, so it does not decode one.
sessionProbeClient :: Text -> Text -> Maybe Text -> EulerClient Aeson.Value
sessionProbeClient = client (Proxy @(SessionAPI Aeson.Value))

callRider :: (BAPFlow m r, SanitizedUrl api, ToJSON a) => BaseUrl -> Text -> EulerClient a -> Proxy api -> m a
callRider internalUrl desc eulerClient api = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @SharedCabBAPError) Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl eulerClient desc api

getSharedCabRoutes :: BAPFlow m r => Text -> BaseUrl -> Text -> Double -> Double -> m SharedCabRoutesResp
getSharedCabRoutes apiKey internalUrl agencyId lat lon = do
  logInfo $ "CallSharedCabBAP: Getting routes for agencyId: " <> agencyId
  callRider internalUrl "GetSharedCabRoutes" (client (Proxy @RoutesAPI) agencyId lat lon (Just apiKey)) (Proxy @RoutesAPI)

selectSharedCabRoute :: BAPFlow m r => Text -> BaseUrl -> BAPSelectRouteReq -> m SelectRouteResp
selectSharedCabRoute apiKey internalUrl req = do
  logInfo $ "CallSharedCabBAP: Selecting route " <> req.routeCode <> " for driverId: " <> req.driverId
  callRider internalUrl "SelectSharedCabRoute" (client (Proxy @SelectRouteAPI) (Just apiKey) req) (Proxy @SelectRouteAPI)

-- | No live session arrives as SHARED_CAB_SESSION_NOT_FOUND in the error body (not a bare status), so this decodes the APIError
-- envelope and checks the errorCode; every other error re-throws verbatim. It inlines callApiUnwrappingApiError's two halves
-- because the NOT_FOUND branch needs the decoded error value.
getSharedCabSession :: BAPFlow m r => Text -> BaseUrl -> Text -> Text -> m (Maybe SharedCabSession)
getSharedCabSession apiKey internalUrl driverId vehicleNumber = do
  logInfo $ "CallSharedCabBAP: Getting session for driverId: " <> driverId
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  newInternalUrl <-
    HM.foldrWithKey
      ( \k v acc ->
          if T.isInfixOf (showBaseUrl k) (showBaseUrl internalUrl)
            then parseBaseUrl (T.replace (showBaseUrl k) (showBaseUrl v) (showBaseUrl internalUrl))
            else acc
      )
      (pure internalUrl)
      internalEndPointHashMap
  res <- EC.callApiExtractingApiError Nothing newInternalUrl (sessionClient driverId vehicleNumber (Just apiKey)) "GetSharedCabSession" (Proxy @(SessionAPI (Maybe SharedCabSession)))
  case res of
    Right mbSession -> pure mbSession
    Left (CallAPIError.RawError clientError) -> throwError $ ExternalAPICallError (Just "BAP_INTERNAL_API_ERROR") internalUrl clientError
    Left (CallAPIError.APIError err)
      | fromBAPErrorCode err == sessionNotFoundErrorCode -> pure Nothing
      | otherwise -> throwError err

sessionNotFoundErrorCode :: Text
sessionNotFoundErrorCode = "SHARED_CAB_SESSION_NOT_FOUND"

setSharedCabSeats :: BAPFlow m r => Text -> BaseUrl -> BAPSeatsReq -> m SharedCabSession
setSharedCabSeats apiKey internalUrl req = do
  logInfo $ "CallSharedCabBAP: Setting seats for driverId: " <> req.driverId
  callRider internalUrl "SetSharedCabSeats" (client (Proxy @SeatsAPI) (Just apiKey) req) (Proxy @SeatsAPI)

endSharedCabRoute :: BAPFlow m r => Text -> BaseUrl -> BAPEndRouteReq -> m (Maybe SharedCabSession)
endSharedCabRoute apiKey internalUrl req = do
  logInfo $ "CallSharedCabBAP: Ending route (next = " <> show req.next <> ") for driverId: " <> req.driverId
  callRider internalUrl "EndSharedCabRoute" (client (Proxy @EndRouteAPI) (Just apiKey) req) (Proxy @EndRouteAPI)

resumeSharedCab :: BAPFlow m r => Text -> BaseUrl -> BAPResumeReq -> m SharedCabSession
resumeSharedCab apiKey internalUrl req = do
  logInfo $ "CallSharedCabBAP: Resuming session for driverId: " <> req.driverId
  callRider internalUrl "ResumeSharedCab" (client (Proxy @ResumeAPI) (Just apiKey) req) (Proxy @ResumeAPI)

getSharedCabTrips :: BAPFlow m r => Text -> BaseUrl -> Text -> Text -> m SharedCabTripsResp
getSharedCabTrips apiKey internalUrl driverId date = do
  logInfo $ "CallSharedCabBAP: Getting trips for driverId: " <> driverId <> ", date: " <> date
  callRider internalUrl "GetSharedCabTrips" (client (Proxy @TripsAPI) driverId date (Just apiKey)) (Proxy @TripsAPI)

postBookingAction :: BAPFlow m r => Text -> BaseUrl -> Text -> Text -> BAPDriverReq -> m SharedCabSession
postBookingAction apiKey internalUrl action bookingId req = do
  logInfo $ "CallSharedCabBooking: " <> action <> " booking " <> bookingId <> " by driver " <> req.driverId
  callRider internalUrl ("SharedCabBooking:" <> action) (client (Proxy @BookingActionAPI) bookingId action (Just apiKey) req) (Proxy @BookingActionAPI)

postCabFull :: BAPFlow m r => Text -> BaseUrl -> BAPDriverReq -> m SharedCabSession
postCabFull apiKey internalUrl req = do
  logInfo $ "CallSharedCabBooking: cab full on " <> req.vehicleNumber <> " by driver " <> req.driverId
  callRider internalUrl "SharedCabCabFull" (client (Proxy @CabFullAPI) (Just apiKey) req) (Proxy @CabFullAPI)
