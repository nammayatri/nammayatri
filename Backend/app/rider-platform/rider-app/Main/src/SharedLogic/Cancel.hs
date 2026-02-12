{-# OPTIONS_GHC -Wno-orphans #-}

module SharedLogic.Cancel where

import qualified Beckn.ACL.Cancel as CACL
import qualified Control.Lens as L
import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as A
import Data.Aeson.Types (parseFail, typeMismatch)
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict.InsOrd as HMSIO
import Data.Maybe ()
import Data.OpenApi hiding (name)
import qualified Data.Text as T
import Domain.Action.UI.Cancel as DCancel
import qualified Domain.Action.UI.Search as DSearch
import Domain.Types.Booking
import qualified Domain.Types.Estimate as DEstimate
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as DPerson
import Domain.Types.ServiceTierType ()
import qualified Kernel.Beam.Functions as B
import Kernel.External.Slack.Types (SlackConfig)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter
import Kernel.Utils.Common
import SharedLogic.BPPFlowRunner (withDirectBPP)
import qualified SharedLogic.CallBPP as CallBPP
import qualified SharedLogic.DirectBPPCall as DirectBPPCall
import qualified Storage.Queries.Booking as QRB
import qualified Storage.Queries.Estimate as QEstimate
import Tools.Error
import TransactionLogs.Types

data CancelAPIResponse = BookingAlreadyCreated | FailedToCancel | Success
  deriving stock (Generic, Show, Enum, Bounded)

allCancelAPIResponse :: [CancelAPIResponse]
allCancelAPIResponse = [minBound .. maxBound]

instance ToJSON CancelAPIResponse where
  toJSON Success = A.object ["result" .= ("Success" :: Text)]
  toJSON BookingAlreadyCreated = A.object ["result" .= ("BookingAlreadyCreated" :: Text)]
  toJSON FailedToCancel = A.object ["result" .= ("FailedToCancel" :: Text)]

instance FromJSON CancelAPIResponse where
  parseJSON (A.Object obj) = do
    result :: String <- obj .: "result"
    case result of
      "FailedToCancel" -> pure FailedToCancel
      "BookingAlreadyCreated" -> pure BookingAlreadyCreated
      "Success" -> pure Success
      _ -> parseFail "Expected \"Success\" in \"result\" field."
  parseJSON err = typeMismatch "Object APISuccess" err

instance ToSchema CancelAPIResponse where
  declareNamedSchema _ = do
    return $
      NamedSchema (Just "CancelAPIResponse") $
        mempty
          & type_ L.?~ OpenApiObject
          & properties
            L..~ HMSIO.singleton "result" enumsSchema
          & required L..~ ["result"]
    where
      enumsSchema =
        (mempty :: Schema)
          & type_ L.?~ OpenApiString
          & enum_ L.?~ map (A.String . T.pack . show) allCancelAPIResponse
          & Inline

cancelSearchUtil ::
  ( DSearch.SearchRequestFlow m r,
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    Redis.HedisFlow m r,
    HasFlowEnv m r '["slackCfg" ::: SlackConfig],
    HasFlowEnv m r '["searchRateLimitOptions" ::: APIRateLimitOptions],
    HasFlowEnv m r '["searchLimitExceedNotificationTemplate" ::: Text],
    MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    CacheFlow m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HM.HashMap KeyConfig TokenConfig],
    EsqDBFlow m r,
    HasField "shortDurationRetryCfg" r RetryCfg,
    HasFlowEnv m r '["nwAddress" ::: BaseUrl]
  ) =>
  (Id DPerson.Person, Id Merchant.Merchant) ->
  Id DEstimate.Estimate ->
  m CancelAPIResponse
cancelSearchUtil (personId, merchantId) estimateId = do
  estimate <- QEstimate.findById estimateId >>= fromMaybeM (EstimateDoesNotExist estimateId.getId)
  activeBooking <- B.runInReplica $ QRB.findByTransactionIdAndStatus estimate.requestId.getId activeBookingStatus
  if isJust activeBooking
    then do
      logTagInfo "Booking already created while cancelling estimate." estimateId.getId
      throwError (ActiveBookingPresent estimateId.getId)
    else do
      dCancelSearch <- DCancel.mkDomainCancelSearch personId estimateId
      result <-
        withTryCatch "cancelSearchBPP" $
          when dCancelSearch.sendToBpp $ do
            cancelBecknReq <- CACL.buildCancelSearchReqV2 dCancelSearch
            withDirectBPP
              (\rt -> DirectBPPCall.directCancel rt merchantId cancelBecknReq)
              (void . withShortRetry $ CallBPP.cancelV2 merchantId dCancelSearch.providerUrl cancelBecknReq)
      case result of
        Left err -> do
          logTagInfo "Failed to cancel" $ show err
          throwError (FailedToCancelSearch estimateId.getId)
        Right _ -> do
          DCancel.cancelSearch personId dCancelSearch
          pure Success
