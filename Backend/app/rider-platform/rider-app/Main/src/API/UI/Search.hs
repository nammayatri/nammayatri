{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module API.UI.Search
  ( DSearch.SearchReq (..),
    DSearch.SearchRes (..),
    DSearch.SearchResp (..),
    DSearch.OneWaySearchReq (..),
    DSearch.RentalSearchReq (..),
    DSearch.SearchReqLocation (..),
    API,
    search',
    search,
    handler,
  )
where

import qualified API.UI.Select as Select
import qualified Beckn.ACL.Cancel as ACL
import qualified Beckn.ACL.Search as TaxiACL
import Data.Aeson
import qualified Data.Text as T
import qualified Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Search as DSearch
import qualified Domain.Types.Booking as Booking
import qualified Domain.Types.BookingCancellationReason as SBCR
import qualified Domain.Types.CancellationReason as SCR
import qualified Domain.Types.Client as DC
import qualified Domain.Types.Estimate as Estimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import qualified Domain.Types.SearchRequest as SearchRequest
import Environment
import Kernel.External.Maps.Google.MapsClient.Types
import qualified Kernel.External.MultiModal.Interface as MultiModal
import Kernel.External.MultiModal.Interface.Types
import qualified Kernel.External.Slack.Flow as SF
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Common hiding (id)
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.Utils.SlidingWindowLimiter
import Kernel.Utils.Version
import qualified Lib.JourneyModule.Base as JM
import qualified Lib.JourneyModule.Types as JMTypes
import Servant hiding (throwError)
import qualified SharedLogic.CallBPP as CallBPP
import SharedLogic.Search as DSearch
import Storage.Beam.SystemConfigs ()
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Estimate as QEstimate
import qualified Storage.Queries.Person as Person
import qualified Storage.Queries.Ride as QR
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Auth
import Tools.Error
import qualified Tools.MultiModal as TMultiModal

-------- Search Flow --------

type API =
  "rideSearch"
    :> TokenAuth
    :> ReqBody '[JSON] DSearch.SearchReq
    :> Header "x-bundle-version" Version
    :> Header "x-client-version" Version
    :> Header "x-config-version" Version
    :> Header "x-rn-version" Text
    :> Header "client-id" (Id DC.Client)
    :> Header "x-device" Text
    :> Header "is-dashboard-request" Bool
    :> Post '[JSON] DSearch.SearchResp

handler :: FlowServer API
handler = search

search :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> FlowHandler DSearch.SearchResp
search (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice = withFlowHandlerAPI . search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice

search' :: (Id Person.Person, Id Merchant.Merchant) -> DSearch.SearchReq -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe (Id DC.Client) -> Maybe Text -> Maybe Bool -> Flow DSearch.SearchResp
search' (personId, merchantId) req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice mbIsDashboardRequest = withPersonIdLogTag personId $ do
  checkSearchRateLimit personId
  fork "updating person versions" $ updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice
  merchant <- CQM.findById (cast merchantId) >>= fromMaybeM (MerchantNotFound merchantId.getId)
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --BEGIN
  when merchant.enableForMultipleSearchIssue $ do
    mbSReq <- QSearchRequest.findLastSearchRequestInKV personId
    shouldCancelPrevSearch <- maybe (return False) (checkValidSearchReq merchant.scheduleRideBufferTime) mbSReq
    when shouldCancelPrevSearch $ do
      fork "handle multiple search request issue" $ do
        case mbSReq of
          Just sReq -> do
            mbEstimate <- QEstimate.findBySRIdAndStatusesInKV sReq.id [Estimate.DRIVER_QUOTE_REQUESTED, Estimate.GOT_DRIVER_QUOTE]
            case mbEstimate of
              Just estimate -> do
                resp <- try @_ @SomeException $ Select.cancelSearch' (personId, merchantId) estimate.id
                case resp of
                  Left _ -> void $ handleBookingCancellation merchantId personId sReq.id
                  Right _ -> pure ()
              Nothing -> void $ handleBookingCancellation merchantId personId sReq.id
          _ -> pure ()
  -- TODO : remove this code after multiple search req issue get fixed from frontend
  --END
  dSearchRes <- DSearch.search personId req mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbClientId mbDevice (fromMaybe False mbIsDashboardRequest) Nothing
  fork "search cabs" . withShortRetry $ do
    becknTaxiReqV2 <- TaxiACL.buildSearchReqV2 dSearchRes
    let generatedJson = encode becknTaxiReqV2
    logDebug $ "Beckn Taxi Request V2: " <> T.pack (show generatedJson)
    void $ CallBPP.searchV2 dSearchRes.gatewayUrl becknTaxiReqV2 merchantId
  fork "Multimodal Search" $ do
    let merchantOperatingCityId = dSearchRes.searchRequest.merchantOperatingCityId
    riderConfig <- QRC.findByMerchantOperatingCityIdInRideFlow merchantOperatingCityId dSearchRes.searchRequest.configInExperimentVersions >>= fromMaybeM (RiderConfigNotFound merchantOperatingCityId.getId)
    when riderConfig.makeMultiModalSearch $
      case req of
        OneWaySearch searchReq -> multiModalSearch searchReq dSearchRes.searchRequest merchantOperatingCityId riderConfig.maximumWalkDistance
        _ -> pure ()
  return $ DSearch.SearchResp dSearchRes.searchRequest.id dSearchRes.searchRequestExpiry dSearchRes.shortestRouteInfo
  where
    -- TODO : remove this code after multiple search req issue get fixed from frontend
    --BEGIN
    checkValidSearchReq scheduleRideBufferTime sReq = do
      now <- getCurrentTime
      let isNonScheduled = diffUTCTime sReq.startTime sReq.createdAt < scheduleRideBufferTime
          isValid = sReq.validTill > now
      return $ isNonScheduled && isValid

handleBookingCancellation :: Id Merchant.Merchant -> Id Person.Person -> Id SearchRequest.SearchRequest -> Flow ()
handleBookingCancellation merchantId _personId sReqId = do
  mbBooking <- QBooking.findByTransactionIdAndStatus sReqId.getId Booking.activeBookingStatus
  case mbBooking of
    Just booking -> do
      let reasonCode = SCR.CancellationReasonCode "multiple search request issue"
          reasonStage = SCR.OnSearch
      let cancelReq =
            DCancel.CancelReq
              { additionalInfo = Nothing,
                reallocate = Just False,
                blockOnCancellationRate = Nothing,
                ..
              }
      mRide <- QR.findActiveByRBId booking.id
      dCancelRes <- DCancel.cancel booking mRide cancelReq SBCR.ByUser
      void $ withShortRetry $ CallBPP.cancelV2 merchantId dCancelRes.bppUrl =<< ACL.buildCancelReqV2 dCancelRes cancelReq.reallocate
    _ -> pure ()

-- TODO : remove this code after multiple search req issue get fixed from frontend
--END

multiModalSearch ::
  OneWaySearchReq ->
  SearchRequest.SearchRequest ->
  Id DMOC.MerchantOperatingCity ->
  Meters ->
  Flow ()
multiModalSearch searchReq searchRequest merchantOperatingCityId maximumWalkDistance = do
  let transitRoutesReq =
        GetTransitRoutesReq
          { origin = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = searchReq.origin.gps.lat, longitude = searchReq.origin.gps.lon}}},
            destination = WayPointV2 {location = LocationV2 {latLng = LatLngV2 {latitude = searchReq.destination.gps.lat, longitude = searchReq.destination.gps.lon}}},
            arrivalTime = Nothing,
            departureTime = searchReq.startTime,
            mode = Nothing,
            transitPreferences = Nothing,
            transportModes = Nothing
          }
  transitServiceReq <- TMultiModal.getTransitServiceReq searchRequest.merchantId merchantOperatingCityId
  otpResponse <- MultiModal.getTransitRoutes transitServiceReq transitRoutesReq >>= fromMaybeM (InternalError "routes dont exist")
  logDebug $ "[Multimodal - OTP Response]" <> show otpResponse
  forM_ otpResponse.routes $ \r -> do
    let initReq =
          JMTypes.JourneyInitData
            { parentSearchId = searchRequest.id,
              merchantId = searchRequest.merchantId,
              merchantOperatingCityId,
              personId = searchRequest.riderId,
              legs = r.legs,
              estimatedDistance = r.distance,
              estimatedDuration = r.duration,
              startTime = r.startTime,
              endTime = r.endTime,
              maximumWalkDistance
            }
    QSearchRequest.updateHasMultimodalSearch (Just True) searchRequest.id
    JM.init initReq

checkSearchRateLimit ::
  ( Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text]
  ) =>
  Id Person.Person ->
  m ()
checkSearchRateLimit personId = do
  let key = searchHitsCountKey personId
  hitsLimit <- asks (.searchRateLimitOptions.limit)
  limitResetTimeInSec <- asks (.searchRateLimitOptions.limitResetTimeInSec)
  unlessM (slidingWindowLimiter key hitsLimit limitResetTimeInSec) $ do
    msgTemplate <- asks (.searchLimitExceedNotificationTemplate)
    let message = T.replace "{#cust-id#}" (getId personId) msgTemplate
    _ <- SF.postMessage message
    throwError $ HitsLimitError limitResetTimeInSec

searchHitsCountKey :: Id Person.Person -> Text
searchHitsCountKey personId = "BAP:Ride:search:" <> getId personId <> ":hitsCount"

updateVersions :: (CacheFlow m r, EsqDBFlow m r, HasFlowEnv m r '["version" ::: DeploymentVersion]) => Id Person.Person -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> m ()
updateVersions personId mbBundleVersion mbClientVersion mbClientConfigVersion mbRnVersion mbDevice = do
  person <- Person.findById personId >>= fromMaybeM (PersonNotFound $ getId personId)
  deploymentVersion <- asks (.version)
  void $ Person.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion (getDeviceFromText mbDevice) deploymentVersion.getDeploymentVersion mbRnVersion
