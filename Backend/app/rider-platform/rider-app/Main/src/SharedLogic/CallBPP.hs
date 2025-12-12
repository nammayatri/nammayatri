{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CallBPP where

import qualified Beckn.ACL.Track as TrackACL
import qualified Beckn.Types.Core.Metro.API.Search as MigAPI
import Beckn.Types.Core.Taxi.API.Cancel as API
import Beckn.Types.Core.Taxi.API.Confirm as API
import qualified Beckn.Types.Core.Taxi.API.Init as API
import Beckn.Types.Core.Taxi.API.Rating as API
import qualified Beckn.Types.Core.Taxi.API.Search as API
import Beckn.Types.Core.Taxi.API.Select as API
import Beckn.Types.Core.Taxi.API.Status as API
import Beckn.Types.Core.Taxi.API.Track as API
import Beckn.Types.Core.Taxi.API.Update as API
import qualified Data.HashMap.Strict as HM
import qualified Data.UUID as UUID
import qualified Domain.Types.Booking as DB
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Ride as DRide
import qualified EulerHS.Types as Euler
import GHC.Records.Extra
import qualified Kernel.External.Maps.Types as MapSearch
import Kernel.Prelude
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Types.Beckn.ReqTypes
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.BecknAPIError (IsBecknAPI)
import Kernel.Utils.InternalAPICallLogging as ApiCallLogger
import Kernel.Utils.Monitoring.Prometheus.Servant (SanitizedUrl)
import Kernel.Utils.Servant.SignatureAuth
import Servant hiding (throwError)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Tools.Error
import Tools.Metrics (CoreMetrics)
import TransactionLogs.PushLogs
import TransactionLogs.Types

searchV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  API.SearchReqV2 ->
  Id Merchant.Merchant ->
  m API.SearchRes
searchV2 gatewayUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.searchReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  res <- callBecknAPIWithSignature' merchantId bapId "search" API.searchAPIV2 gatewayUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.searchReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "searchV2" "BAP" transactionId (Just req) res
  pure res

searchMetro ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  BaseUrl ->
  BecknReq MigAPI.SearchIntent ->
  m ()
searchMetro gatewayUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  void $ callBecknAPIWithSignatureMetro "search" MigAPI.searchAPI gatewayUrl internalEndPointHashMap req

selectV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  API.SelectReqV2 ->
  Id Merchant.Merchant ->
  m API.SelectRes
selectV2 providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- req.selectReqContext.contextBapId & fromMaybeM (InvalidRequest "BapId is missing")
  res <- callBecknAPIWithSignature' merchantId bapId "select" API.selectAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.selectReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "selectV2" "BAP" transactionId (Just req) res
  pure res

initV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  API.InitReqV2 ->
  Id Merchant.Merchant ->
  m API.InitRes
initV2 providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.initReqContext.contextBapId
  res <- callBecknAPIWithSignature' merchantId bapId "init" API.initAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.initReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "initV2" "BAP" transactionId (Just req) res
  pure res

confirmV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBFlow m r
  ) =>
  BaseUrl ->
  ConfirmReqV2 ->
  Id Merchant.Merchant ->
  m ConfirmRes
confirmV2 providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.confirmReqContext.contextBapId
  res <- callBecknAPIWithSignature' merchantId bapId "confirm" API.confirmAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.confirmReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "confirmV2" "BAP" transactionId (Just req) res
  pure res

cancelV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r
  ) =>
  Id Merchant.Merchant ->
  BaseUrl ->
  CancelReqV2 ->
  m CancelRes
cancelV2 merchantId providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.cancelReqContext.contextBapId
  res <- callBecknAPIWithSignature' merchantId bapId "cancel" API.cancelAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.cancelReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "cancelV2" "BAP" transactionId (Just req) res
  pure res

update ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  BaseUrl ->
  UpdateReq ->
  m UpdateRes
update providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callBecknAPIWithSignature req.context.bap_id "update" API.updateAPIV1 providerUrl internalEndPointHashMap req

updateV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasRequestId r
  ) =>
  BaseUrl ->
  UpdateReqV2 ->
  m UpdateRes
updateV2 providerUrl req = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.updateReqContext.contextBapId
  res <- callBecknAPIWithSignature bapId "update" API.updateAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.updateReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "updateV2" "BAP" transactionId (Just req) res
  pure res

callTrack ::
  ( HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    MonadFlow m,
    CoreMetrics m,
    EsqDBFlow m r,
    CacheFlow m r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig]
  ) =>
  DB.Booking ->
  DRide.Ride ->
  m ()
callTrack booking ride = do
  merchant <- CQM.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  bppBookingId <- booking.bppBookingId & fromMaybeM (InvalidRequest "Bpp Booking is missing")
  let merchantOperatingCityId = booking.merchantOperatingCityId
  city <- CQMOC.findById merchantOperatingCityId >>= fmap (.city) . fromMaybeM (MerchantOperatingCityNotFound merchantOperatingCityId.getId)
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  let trackBuildReq =
        TrackACL.TrackBuildReq
          { bppId = booking.providerId,
            bppUrl = booking.providerUrl,
            transactionId = booking.transactionId,
            bppRideId = ride.bppRideId,
            ..
          }
  trackBecknReq <- TrackACL.buildTrackReqV2 trackBuildReq
  res <- callBecknAPIWithSignature' booking.merchantId merchant.bapId "track" API.trackAPIV2 booking.providerUrl internalEndPointHashMap trackBecknReq
  fork ("Logging Internal API Call") $ do
    ApiCallLogger.pushInternalApiCallDataToKafka "callTrack" "BAP" (Just booking.transactionId) (Just trackBecknReq) res
  pure ()

data GetLocationRes = GetLocationRes
  { currPoint :: MapSearch.LatLong,
    lastUpdate :: UTCTime
  }
  deriving (Show, Generic, ToJSON, FromJSON, ToSchema)

callGetDriverLocation ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Maybe BaseUrl ->
  m GetLocationRes
callGetDriverLocation mTrackingUrl = do
  trackingUrl <- mTrackingUrl & fromMaybeM (RideFieldNotPresent "trackingUrl")
  let eulerClient = Euler.client (Proxy @(Get '[JSON] GetLocationRes))
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  callApiUnwrappingApiError (identity @TrackUrlError) Nothing (Just "TRACK_URL_NOT_AVAILABLE") (Just internalEndPointHashMap) trackingUrl eulerClient "BPP.driverTrackUrl" (Proxy @(Get '[JSON] GetLocationRes))

feedbackV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasRequestId r
  ) =>
  BaseUrl ->
  RatingReqV2 ->
  Id Merchant.Merchant ->
  m RatingRes
feedbackV2 providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.ratingReqContext.contextBapId
  res <- callBecknAPIWithSignature' merchantId bapId "feedback" API.ratingAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.ratingReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "feedbackV2" "BAP" transactionId (Just req) res
  pure res

callStatusV2 ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    EsqDBFlow m r,
    HasRequestId r
  ) =>
  BaseUrl ->
  StatusReqV2 ->
  Id Merchant.Merchant ->
  m StatusRes
callStatusV2 providerUrl req merchantId = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  bapId <- fromMaybeM (InvalidRequest "BapId is missing") req.statusReqContext.contextBapId
  res <- callBecknAPIWithSignature' merchantId bapId "status" API.statusAPIV2 providerUrl internalEndPointHashMap req
  fork ("Logging Internal API Call") $ do
    let transactionId = req.statusReqContext.contextTransactionId <&> UUID.toText
    ApiCallLogger.pushInternalApiCallDataToKafka "statusV2" "BAP" transactionId (Just req) res
  pure res

callBecknAPIWithSignature ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature a = callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing

callBecknAPIWithSignature' ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    ToJSON req,
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasRequestId r
  ) =>
  Id Merchant.Merchant ->
  Text ->
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignature' merchantId a b c d e req' = do
  fork ("sending " <> show b <> ", pushing ondc logs") do
    void $ pushLogs b (toJSON req') merchantId.getId "MOBILITY"
  callBecknAPI (Just $ Euler.ManagerSelector $ getHttpManagerKey a) Nothing b c d e req'

callBecknAPIWithSignatureMetro ::
  ( MonadFlow m,
    CoreMetrics m,
    IsBecknAPI api req res,
    SanitizedUrl api,
    HasRequestId r,
    MonadReader r m
  ) =>
  Text ->
  Proxy api ->
  BaseUrl ->
  HM.HashMap BaseUrl BaseUrl ->
  req ->
  m res
callBecknAPIWithSignatureMetro a b c d e = do
  -- bapId <- asks (.bapSelfIds.metro)
  callBecknAPI
    Nothing -- (Just $ Euler.ManagerSelector $ getHttpManagerKey bapId)
    Nothing
    a
    b
    c
    d
    e
