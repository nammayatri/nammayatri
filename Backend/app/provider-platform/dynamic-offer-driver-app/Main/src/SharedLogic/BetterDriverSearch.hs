{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.BetterDriverSearch
  ( startBetterDriverSearch,
  )
where

import qualified Control.Monad.Catch as C
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Action.UI.SearchRequestForDriver as USRD
import qualified Domain.Types.Booking as SRB
import qualified Domain.Types.ConditionalCharges as DCC
import qualified Domain.Types.Estimate as DEst
import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.Merchant as DMerc
import qualified Domain.Types.Ride as DRide
import qualified Domain.Types.SearchRequest as DSR
import qualified Domain.Types.SearchTry as DST
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Storage.Clickhouse.Config as CH
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (HasKafkaProducer, KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics (CoreMetrics, DeploymentVersion)
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import qualified Lib.Finance.Core.Types as Finance
import Lib.Finance.Storage.Beam.BeamFlow (BeamFlow)
import Lib.Scheduler (SchedulerType)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import Lib.SessionizerMetrics.Types.Event (EventStreamFlow)
import SharedLogic.Allocator (CheckBetterDriverSearchProximityJobData (..))
import SharedLogic.Allocator.Jobs.CheckBetterDriverSearchProximity (betterDriverSearchProximityTickIntervalSec)
import SharedLogic.Allocator.Jobs.SendSearchRequestToDrivers (sendSearchRequestToDrivers')
import qualified SharedLogic.CallBAPInternal as CallBAPInternal
import qualified SharedLogic.DriverPool as DP
import qualified SharedLogic.DriverPool.Types as SDT
import qualified SharedLogic.External.LocationTrackingService.Types as LT
import SharedLogic.GoogleTranslate (TranslateFlow)
import SharedLogic.MerchantPaymentMethod
import SharedLogic.SearchTry (buildTripQuoteDetail, initiateDriverSearchBatch)
import qualified SharedLogic.Type as SLT
import qualified Storage.CachedQueries.Merchant.MerchantPaymentMethod as QMPM
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverQuote as QDQ
import qualified Storage.Queries.Estimate as QEst
import qualified Storage.Queries.RiderDetails as QRD
import qualified Storage.Queries.SearchRequest as QSR
import qualified Storage.Queries.SearchTry as QST
import Tools.Error
import qualified Tools.Metrics as Metrics
import TransactionLogs.Types (KeyConfig, TokenConfig)

-- | Kicks off a stand-by SearchTry on the booking's existing SearchRequest - the
-- current driver's Ride is never touched here. Resolution (a better driver accepting,
-- the current driver closing in on pickup, or a timeout) is handled elsewhere; this
-- function only ever starts the search.
--
-- Mirrors SharedLogic.Cancel.reAllocateBookingIfPossible's dynamic-offer path (same
-- booking -> quote -> searchTry -> searchRequest lookup, same blacklist call, same
-- TripQuoteDetail construction) but deliberately does not touch or share code with
-- it: that function's job is "the current driver is already gone, replace them,"
-- ours is "the current driver is fine, just also look for someone better" - keeping
-- them separate avoids any risk of this feature changing that existing function's
-- behaviour.
startBetterDriverSearch ::
  ( EncFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Metrics.HasBPPMetrics m r,
    HasField "searchRequestExpirationSeconds" r NominalDiffTime,
    HasField "version" r DeploymentVersion,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
    HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    Metrics.HasSendSearchRequestToDriverMetrics m r,
    Metrics.HasDriverSearchRequestResponseMetrics m r,
    HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools],
    HasHttpClientOptions r c,
    HasLongDurationRetryCfg r c,
    HasField "singleBatchProcessingTempDelay" r NominalDiffTime,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["ondcTokenHashMap" ::: HMS.HashMap KeyConfig TokenConfig],
    HasFlowEnv m r '["nwAddress" ::: BaseUrl],
    HasFlowEnv m r '["fabricGatewayBaseUrl" ::: BaseUrl],
    TranslateFlow m r,
    LT.HasLocationService m r,
    HasFlowEnv m r '["maxNotificationShards" ::: Int],
    HasShortDurationRetryCfg r c,
    HasKafkaProducer r,
    HasField "enableAPILatencyLogging" r Bool,
    HasField "enableAPIPrometheusMetricLogging" r Bool,
    HasFlowEnv m r '["appBackendBapInternal" ::: CallBAPInternal.AppBackendBapInternal],
    HasField "blackListedJobs" r [Text],
    HasField "enableLtsPoolDataForPooling" r Bool,
    Redis.HedisLTSFlowEnv r,
    ClickhouseFlow m r,
    Finance.HasActorInfo m r,
    Redis.HedisFlow m r,
    BeamFlow m r,
    CoreMetrics m,
    HasField "driverQuoteExpirationSeconds" r NominalDiffTime,
    HasFlowEnv m r '["version" ::: DeploymentVersion],
    EventStreamFlow m r,
    HasPrettyLogger m r,
    ServiceFlow m r,
    HasField "quoteRespondCoolDown" r Int,
    HasField "driverUnlockDelay" r Seconds,
    C.MonadCatch m
  ) =>
  DMerc.Merchant ->
  SRB.Booking ->
  DRide.Ride ->
  m ()
startBetterDriverSearch merchant booking ride = do
  driverQuote <- QDQ.findById (Id booking.quoteId) >>= fromMaybeM (QuoteNotFound booking.quoteId)
  searchTry <- QST.findById driverQuote.searchTryId >>= fromMaybeM (SearchTryNotFound driverQuote.searchTryId.getId)
  searchReq <- QSR.findById searchTry.requestId >>= fromMaybeM (SearchRequestNotFound searchTry.requestId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = booking.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound booking.merchantOperatingCityId.getId)
  let searchBlacklistTtl = fromMaybe 3600 transporterConfig.driverSearchBlacklistDurationSeconds
  DP.addDriverToSearchCancelledList searchBlacklistTtl searchReq.id ride.driverId
  tripQuoteDetails <- createTripQuoteDetails searchReq searchTry driverQuote.estimateId driverQuote.fareParams.conditionalCharges
  merchantPaymentMethod <- maybe (return Nothing) QMPM.findById booking.paymentMethodId
  let paymentMethodInfo = mkPaymentMethodInfo <$> merchantPaymentMethod
  mbRiderDetails <- maybe (pure Nothing) QRD.findById searchReq.riderId
  let driverSearchBatchInput =
        SDT.DriverSearchBatchInput
          { sendSearchRequestToDrivers = sendSearchRequestToDrivers',
            merchant,
            searchReq,
            tripQuoteDetails,
            customerExtraFee = searchTry.customerExtraFee,
            negativeFareAdjustment = searchTry.negativeFareAdjustment,
            messageId = booking.id.getId,
            isRepeatSearch = False,
            isAllocatorBatch = False,
            billingCategory = searchTry.billingCategory,
            paymentMethodInfo = paymentMethodInfo,
            riderDetails = mbRiderDetails,
            emailDomain = searchTry.emailDomain,
            businessEmailDomain = searchTry.businessEmailDomain,
            driverPreference = searchTry.driverPreference,
            addOnData = searchTry.addOnData,
            betterDriverSearchForBookingId = Just booking.id
          }
  newSearchTry <- initiateDriverSearchBatch driverSearchBatchInput
  JC.createJobIn @_ @'CheckBetterDriverSearchProximity (Just merchant.id) (Just booking.merchantOperatingCityId) betterDriverSearchProximityTickIntervalSec $
    CheckBetterDriverSearchProximityJobData
      { searchTryId = newSearchTry.id,
        bookingId = booking.id,
        rideId = ride.id,
        driverId = ride.driverId,
        merchantId = merchant.id
      }
  where
    -- Mirrors SharedLogic.Cancel.reAllocateBookingIfPossible's local helper of the same
    -- name, with `booking` passed explicitly instead of captured from an enclosing scope.
    createTripQuoteDetails ::
      ( MonadFlow m,
        CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
        HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
        HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
        ClickhouseFlow m r
      ) =>
      DSR.SearchRequest ->
      DST.SearchTry ->
      Id DEst.Estimate ->
      [DCC.ConditionalCharges] ->
      m [SDT.TripQuoteDetail]
    createTripQuoteDetails searchReq searchTry estimateId conditionalCharges =
      if length searchTry.estimateIds > 1
        then traverse (createQuoteDetails searchReq searchTry conditionalCharges) searchTry.estimateIds
        else do
          quoteDetail <- createQuoteDetails searchReq searchTry conditionalCharges estimateId.getId
          return [quoteDetail]

    createQuoteDetails ::
      ( MonadFlow m,
        CacheFlow m r,
        EsqDBFlow m r,
        EsqDBReplicaFlow m r,
        HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl],
        HasField "serviceClickhouseCfg" r CH.ClickhouseCfg,
        HasField "serviceClickhouseEnv" r CH.ClickhouseEnv,
        ClickhouseFlow m r
      ) =>
      DSR.SearchRequest ->
      DST.SearchTry ->
      [DCC.ConditionalCharges] ->
      Text ->
      m SDT.TripQuoteDetail
    createQuoteDetails searchReq searchTry conditionalCharges estimateId = do
      estimate <- QEst.findById (Id estimateId) >>= fromMaybeM (EstimateNotFound estimateId)
      let mbDriverExtraFeeBounds = if isJust estimate.driverExtraFeeBounds then estimate.driverExtraFeeBounds else ((,) <$> estimate.estimatedDistance <*> (join $ (.driverExtraFeeBounds) <$> estimate.farePolicy)) <&> \(dist, driverExtraFeeBounds) -> DFP.findDriverExtraFeeBoundsByDistance dist driverExtraFeeBounds
          driverPickUpCharge = join $ USRD.extractDriverPickupCharges <$> ((.farePolicyDetails) <$> estimate.farePolicy)
          driverParkingCharge = join $ (.parkingCharge) <$> estimate.farePolicy
          businessDiscount = if searchTry.billingCategory == SLT.BUSINESS then fromMaybe 0.0 estimate.businessDiscount else 0.0
          personalDiscount = if searchTry.billingCategory == SLT.PERSONAL then fromMaybe 0.0 estimate.personalDiscount else 0.0
      buildTripQuoteDetail searchReq estimate.tripCategory estimate.vehicleServiceTier estimate.vehicleServiceTierName (estimate.minFare + fromMaybe 0 searchTry.customerExtraFee + fromMaybe 0 searchTry.petCharges - businessDiscount - personalDiscount) (Just booking.isDashboardRequest) (mbDriverExtraFeeBounds <&> (.minFee)) (mbDriverExtraFeeBounds <&> (.maxFee)) (mbDriverExtraFeeBounds <&> (.stepFee)) (mbDriverExtraFeeBounds <&> (.defaultStepFee)) driverPickUpCharge driverParkingCharge estimate.id.getId conditionalCharges False ((.congestionCharge) =<< estimate.fareParams) searchTry.petCharges (estimate.fareParams >>= (.priorityCharges)) estimate.commissionCharges booking.fareParams.tollCharges booking.fareParams.govtCharges booking.fareParams.driverCancellationNotAllowed booking.fareParams.bufferedFare
