{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Scheduler.Jobs.CallPoliceApi where

import qualified Data.HashMap.Strict as HM
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.MerchantServiceConfig as DMSC
import qualified Domain.Types.Ride as DRide
import qualified Kernel.Beam.Functions as B
import qualified Kernel.External.IncidentReport as IncidentReport
import Kernel.External.IncidentReport.Interface.Types as IncidentReportTypes
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.External.Tokenize as Tokenize
import Kernel.External.Types (SchedulerFlow)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import SharedLogic.JobScheduler
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantServiceConfig as QMSC
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig)
import qualified Storage.Queries.Booking as QB
import qualified Storage.Queries.Ride as QR
import Tools.Error

sendCallPoliceApi ::
  ( CallApiFlow m r,
    SchedulerFlow r,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]
  ) =>
  Job 'CallPoliceApi ->
  m ExecutionResult
sendCallPoliceApi Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      _personId = jobData.personId
      rideId = jobData.rideId
      jmCode = jobData.jmCode
  ride <- B.runInReplica $ QR.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- B.runInReplica $ QB.findById ride.bookingId >>= fromMaybeM (BookingDoesNotExist ride.bookingId.getId)
  let merchantOperatingCityId = booking.merchantOperatingCityId
      merchantId = booking.merchantId
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist merchantOperatingCityId.getId)
  rideCallPoliceAPIKeyExists <- Hedis.withCrossAppRedis $ Hedis.get (mkRideCallPoliceAPIKey rideId)
  case rideCallPoliceAPIKeyExists of
    Nothing -> do
      logDebug $ "TimeValidation for this ride has been completed." <> show rideId <> "skipping calling police API"
      return Complete
    (Just _ :: Maybe Int) -> do
      logDebug $ "Safety alert Call Police API Job triggered for ride : " <> show rideId
      coordinates <- fetchLatLong ride merchantId
      token <- getTokenofJMService merchantId merchantOperatingCityId
      initiateUpdateIncidentReport merchantId merchantOperatingCityId jmCode token coordinates
      ReSchedule <$> getRescheduledTime riderConfig.policeTriggerDelay

fetchLatLong :: (CallApiFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl]) => DRide.Ride -> Id Merchant.Merchant -> m Maps.LatLong
fetchLatLong ride merchantId = do
  merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
  CallBPPInternal.getDriverCoordinatesDetails merchant.driverOfferApiKey merchant.driverOfferBaseUrl ride.bppRideId.getId >>= maybe (throwError $ InternalError "Failed to fetch Driver Location in Safety flow") pure

initiateUpdateIncidentReport :: CallApiFlow m r => Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> Text -> Text -> Maps.LatLong -> m ()
initiateUpdateIncidentReport merchantId merchantOpCityId jmCode token coordinates = do
  merchantIRSvcCfg <- getServiceConfig merchantId merchantOpCityId (DMSC.IncidentReportService ERSS)
  let incidentReq =
        IncidentReportTypes.IncidentReportUpdateReq
          { latitude = coordinates.lat,
            longitude = coordinates.lon,
            ..
          }
  res <- IncidentReport.reportIncidentUpdate merchantIRSvcCfg incidentReq
  logDebug $ "Incident Report Update Response: " <> show res.status <> " , " <> show res.message <> " , " <> show res.incidentData
  pure ()

getTokenofJMService :: CallApiFlow m r => Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> m Text
getTokenofJMService merchantId merchantOpCityId = do
  Hedis.withCrossAppRedis $ Hedis.get (mkJMTokenKey merchantOpCityId) >>= maybe fetchAndStoreToken pure
  where
    fetchAndStoreToken = do
      logDebug "Journey Monitoring Token does not exist in Redis, fetching from Tokenize Service"
      sc <-
        QMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId (DMSC.TokenizationService Tokenize.JourneyMonitoring)
          >>= maybe (throwError $ MerchantServiceUsageConfigNotFound merchantId.getId) pure
          >>= validateTokenizationServiceConfig
      res <- Tokenize.tokenize sc Tokenize.TokenizationReq {expiry = Nothing, code = Nothing, codeVerifier = Nothing}
      now <- getCurrentTime
      let expirationSeconds = diffUTCTimeInSeconds now (res.expiresAt)
      Hedis.withCrossAppRedis $ Hedis.setExp (mkJMTokenKey merchantOpCityId) res.token expirationSeconds
      logDebug $ "Journey Monitoring Token fetched and stored : " <> show res.token
      pure res.token

    validateTokenizationServiceConfig sc = case sc.serviceConfig of
      DMSC.TokenizationServiceConfig config -> pure config
      _ -> throwError $ InternalError "Service Config is not Tokenization service config."

getRescheduledTime :: MonadTime m => NominalDiffTime -> m UTCTime
getRescheduledTime delay = addUTCTime delay <$> getCurrentTime

mkJMTokenKey :: Id DMOC.MerchantOperatingCity -> Text
mkJMTokenKey merchantOpCityId = "JourneyMonitoring:merchantOpCityId:" <> merchantOpCityId.getId

mkRideCallPoliceAPIKey :: Id DRide.Ride -> Text
mkRideCallPoliceAPIKey rideId = "RideCallPoliceAPI:rideId:" <> rideId.getId

diffUTCTimeInSeconds :: UTCTime -> Maybe UTCTime -> Int
diffUTCTimeInSeconds now (Just expTime) = round $ diffUTCTime (addUTCTime (-3600) expTime) now
diffUTCTimeInSeconds _ Nothing = 3600 -- default to (1 hour)

getServiceConfig :: CallApiFlow m r => Id Merchant.Merchant -> Id DMOC.MerchantOperatingCity -> DMSC.ServiceName -> m IncidentReportServiceConfig
getServiceConfig merchantId merchantOpCityId service = do
  merchantSvcCfgResult <- QMSC.findByMerchantOpCityIdAndService merchantId merchantOpCityId service
  case merchantSvcCfgResult of
    Just cfg -> case cfg.serviceConfig of
      DMSC.IncidentReportServiceConfig sc -> return sc
      _ -> throwError $ InternalError $ "Service Config is not Incident Report service config" <> show service
    Nothing -> throwError $ MerchantServiceUsageConfigNotFound merchantId.getId

type CallApiFlow m r = (MonadFlow m, EsqDBFlow m r, CacheFlow m r, EncFlow m r)
