{-# LANGUAGE TemplateHaskell #-}

-- | Shared-cab "stuck flag" reconciler job (NY shared-cab-prime, task 4.2B).
--
--   One job per (merchant, city), self-re-enqueued at a fixed interval.
--   Purpose: driver_information.shared_cab_session_active is fail-closed
--   (a True row excludes the driver from the plain-taxi pool), so a driver
--   whose rider-app session died without the end-session update reaching us
--   would be locked out forever. The reconciler walks every driver in the
--   city whose flag is True, asks the rider-app BAP for the live shared-cab
--   session (GET /internal/sharedCab/session?driverId&vehicleNumber), and
--   flips the flag to False on 404/exception; a 200 leaves the row alone.
--
--   City gating: TransporterConfig.sharedCabReconcilerEnabled; Nothing/false
--   => this job terminates its own chain (fail-closed, no writes, no BAP
--   spam). The first job is created via the dashboard scheduler trigger
--   (Common.SharedCabReconcilerTrigger) — see
--   Domain.Action.Dashboard.Management.Merchant.postMerchantSchedulerTrigger.
module SharedLogic.Allocator.Jobs.SharedCab.Reconciler
  ( runSharedCabReconcilerJob,
  )
where

import Data.Aeson (Value)
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import EulerHS.Types (EulerClient, client)
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Error.BaseError.HTTPError
import Kernel.Types.Error.BaseError.HTTPError.FromResponse (FromResponse (..))
import Kernel.Utils.Common
import qualified Kernel.Utils.Servant.Client as EC
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import Servant hiding (throwError)
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)

-- Fixed sweep interval; per-city on/off lives on TransporterConfig itself, so
-- cadence stays a code constant for the pilot (make it config if ops asks).
sharedCabReconcilerInterval :: NominalDiffTime
sharedCabReconcilerInterval = 900 -- 15 minutes

sharedCabReconcilerBatchSize :: Int
sharedCabReconcilerBatchSize = 100

-- | BAP error bodies for the internal session API are not parsed — the
--   reconciler only cares 2xx vs anything-else, so the unwrap helper takes a
--   bottom type: every failure response becomes a thrown
--   ExternalAPICallError, caught below per driver.
data SharedCabSessionApiError = SharedCabSessionApiError
  deriving (Eq, Show, IsBecknAPIError)

instance IsBaseError SharedCabSessionApiError

instance IsHTTPError SharedCabSessionApiError where
  toErrorCode SharedCabSessionApiError = "SHARED_CAB_SESSION_API_ERROR"

instance IsAPIError SharedCabSessionApiError

instanceExceptionWithParent 'HTTPException ''SharedCabSessionApiError

instance FromResponse SharedCabSessionApiError where
  fromResponse = const Nothing

-- GET /internal/sharedCab/session?driverId=&vehicleNumber= --------------------
-- Mirrors SharedLogic.CallSharedCabBAP.getSharedCabSession (45 branch), but
-- self-contained so this branch doesn't need the proxy client module: the
-- response payload is opaque (we only need the status), hence Value.

type SharedCabSessionAPI =
  "internal"
    :> "sharedCab"
    :> "session"
    :> QueryParam' '[Required, Strict] "driverId" Text
    :> QueryParam' '[Required, Strict] "vehicleNumber" Text
    :> Header "token" Text
    :> Get '[JSON] Value

callSessionClient :: Text -> Text -> Maybe Text -> EulerClient Value
callSessionClient = client (Proxy @SharedCabSessionAPI)

callSessionAPI :: Proxy SharedCabSessionAPI
callSessionAPI = Proxy

getSharedCabSession ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  m Value
getSharedCabSession apiKey internalUrl driverId vehicleNumber = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  EC.callApiUnwrappingApiError (identity @SharedCabSessionApiError) Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (callSessionClient driverId vehicleNumber (Just apiKey)) "GetSharedCabSession" callSessionAPI

runSharedCabReconcilerJob ::
  forall m r c.
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasFlowEnv m r '["appBackendBapInternal" ::: AppBackendBapInternal],
    HasShortDurationRetryCfg r c,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text],
    JobCreatorEnv r,
    HasSchemaName SchedulerJobT
  ) =>
  Job 'SharedCabReconciler ->
  m ExecutionResult
runSharedCabReconcilerJob Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) $ do
  let jobData :: SharedCabReconcilerJobData = jobInfo.jobData
      merchantId = jobData.merchantId
      merchantOpCityId = jobData.merchantOperatingCityId
  mbTc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  let reconcilerOn = maybe False (fromMaybe False . (.sharedCabReconcilerEnabled)) mbTc
  if not reconcilerOn
    then do
      logInfo $ "SharedCabReconciler disabled for city " <> merchantOpCityId.getId <> "; dropping job chain"
      pure Complete
    else do
      bap <- asks (.appBackendBapInternal)
      (checked, cleared) <- reconcileAll bap.apiKey bap.url 0 (0, 0)
      logInfo $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " checked=" <> show checked <> " cleared=" <> show cleared
      JC.createJobIn @_ @'SharedCabReconciler (Just merchantId) (Just merchantOpCityId) sharedCabReconcilerInterval jobData
      pure Complete
  where
    -- One full paginated pass over the city's flagged drivers; clears the
    -- flag for each driver whose rider-app session is gone (404/ex). A row
    -- that flips True again mid-sweep is harmless: the next fire rechecks it.
    -- Offsets are taken over a filter this pass shrinks in place, so rows can
    -- shift between pages and a row may be rechecked or skipped within one
    -- pass; the 15-minute sweep cadence makes coverage eventual.
    reconcileAll apiKey internalUrl offset (checked, cleared) = do
      flagged <- QDIExtra.findAllSharedCabActiveDrivers jobData.merchantOperatingCityId (Just sharedCabReconcilerBatchSize) (Just offset)
      clearedDelta <- foldM (reconcileDriver apiKey internalUrl) 0 flagged
      let checked' = checked + length flagged
          cleared' = cleared + clearedDelta
      if length flagged < sharedCabReconcilerBatchSize
        then pure (checked', cleared')
        else reconcileAll apiKey internalUrl (offset + sharedCabReconcilerBatchSize) (checked', cleared')

    reconcileDriver apiKey internalUrl cleared driverInfo = do
      let driverId = driverInfo.driverId
      mbVehicle <- QVeh.findById driverId
      clearedNow <- case mbVehicle of
        Nothing -> do
          -- No vehicle => no plate => no live session can start; clear the flag.
          logWarning $ "SharedCabReconciler: driver " <> driverId.getId <> " flagged but has no vehicle; clearing flag"
          clearFlag driverId
        Just vehicle -> do
          resp <- withTryCatch "getSharedCabSession:sharedCabReconciler" $ getSharedCabSession apiKey internalUrl driverId.getId vehicle.registrationNo
          case resp of
            Right _ -> do
              logDebug $ "SharedCabReconciler: session live for driver " <> driverId.getId
              pure False
            Left err -> do
              logWarning $ "SharedCabReconciler: session check failed for driver " <> driverId.getId <> ": " <> show err <> "; clearing flag"
              clearFlag driverId
      pure $ if clearedNow then cleared + 1 else cleared

    clearFlag driverId = do
      QDIExtra.updateSharedCabSessionActive False driverId
      pure True
