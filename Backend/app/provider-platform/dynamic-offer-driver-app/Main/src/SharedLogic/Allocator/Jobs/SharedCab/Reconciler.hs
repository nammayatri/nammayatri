-- | Shared-cab "stuck flag" reconciler: one self-re-enqueuing job per (merchant, city). The fail-closed shared_cab_session_active flag
-- is cleared ONLY on a decoded BAP error body (SHARED_CAB_SESSION_NOT_FOUND or ..._HELD_BY_ANOTHER_DRIVER); a bare 404/409, 2xx, other code, timeout or 5xx keeps it.
module SharedLogic.Allocator.Jobs.SharedCab.Reconciler
  ( runSharedCabReconcilerJob,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HMS
import qualified Data.Map as M
import qualified Domain.Types.DriverInformation as DDI
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Kernel.Beam.Lib.UtilsTH (HasSchemaName)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import Kernel.Utils.Error.BaseError.HTTPError.APIError (APICallError (..))
import qualified Kernel.Utils.Servant.Client as EC
import Lib.ConfigPilot.Interface.Types (getOneConfig)
import Lib.Scheduler
import Lib.Scheduler.JobStorageType.DB.Table (SchedulerJobT)
import qualified Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import SharedLogic.SharedCab.RiderClient (SessionAPI, sessionProbeClient)
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)

sharedCabReconcilerBatchSize :: Int
sharedCabReconcilerBatchSize = 100

clearSignalErrorCodes :: [Text]
clearSignalErrorCodes = ["SHARED_CAB_SESSION_NOT_FOUND", "SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER"]

data SessionProbe
  = -- | Clear, with the decoded errorCode for the audit log line.
    SessionGone Text
  | -- | No unambiguous end signal — leave the flag untouched.
    SessionUnknown
  deriving (Eq, Show)

-- | A failure body that decodes to the kernel APIError contract is a typed APICallError; every other failure lands on SessionUnknown.
probeSharedCabSession ::
  ( MonadFlow m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl],
    HasRequestId r
  ) =>
  Text ->
  BaseUrl ->
  Text ->
  Text ->
  m SessionProbe
probeSharedCabSession apiKey internalUrl driverId vehicleNumber = do
  internalEndPointHashMap <- asks (.internalEndPointHashMap)
  eResp <-
    withTryCatch "getSharedCabSession:sharedCabReconciler" $
      EC.callApiUnwrappingApiError APICallError Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (sessionProbeClient driverId vehicleNumber (Just apiKey)) "GetSharedCabSession" (Proxy @(SessionAPI Aeson.Value))
  pure $ case eResp of
    Right _ -> SessionUnknown -- 2xx: a live session answered; nothing to do
    Left exc
      | Just (APICallError apiErr) <- fromException @APICallError exc,
        apiErr.errorCode `elem` clearSignalErrorCodes ->
        SessionGone apiErr.errorCode
      | otherwise -> SessionUnknown -- everything else: fail-closed, keep the flag

type ReconcileFlow m r =
  ( EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r,
    MonadFlow m,
    MonadIO m,
    CoreMetrics m,
    HasFlowEnv m r '["internalEndPointHashMap" ::: HMS.HashMap BaseUrl BaseUrl]
  )

runSharedCabReconcilerJob ::
  forall m r c.
  ( ReconcileFlow m r,
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
      merchantOpCityId = jobData.merchantOperatingCityId
  -- Heartbeat BEFORE the enabled check, so even a disabled fire refreshes the seed marker. Best-effort: must not kill the chain.
  withTryCatch "sharedCabReconciler:refreshSeedMarker" (SharedCabSeed.refreshSharedCabReconcilerSeedMarker merchantOpCityId)
    >>= either (\err -> logError $ "SharedCabReconciler: seed marker refresh failed for city " <> merchantOpCityId.getId <> ": " <> show err) pure
  mbTc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  let reconcilerOn = maybe False (fromMaybe False . (.sharedCabReconcilerEnabled)) mbTc
  if reconcilerOn
    then sweepAndReenqueue jobData
    else dropChain merchantOpCityId
  pure Complete

sweepAndReenqueue ::
  ( ReconcileFlow m r,
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
  SharedCabReconcilerJobData ->
  m ()
sweepAndReenqueue jobData = do
  let merchantOpCityId = jobData.merchantOperatingCityId
  bap <- asks (.appBackendBapInternal)
  eSweep <- withTryCatch "sharedCabReconciler:sweep" $ reconcileAll merchantOpCityId bap.apiKey bap.url Nothing (0 :: Int, 0 :: Int, 0 :: Int)
  case eSweep of
    Right (checked, cleared, leftFlag) ->
      logInfo $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " checked=" <> show checked <> " cleared=" <> show cleared <> " left=" <> show leftFlag
    Left err ->
      logError $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " sweep aborted: " <> show err <> "; re-enqueueing, un-reconciled flags stay as they were"
  JC.createJobIn @_ @'SharedCabReconciler (Just jobData.merchantId) (Just merchantOpCityId) SharedCabSeed.sharedCabReconcilerInterval jobData

-- | The chain is intentionally ending: release the seed marker. Best-effort: a DEL failure must not throw; the key is logged for manual cleanup.
dropChain :: ReconcileFlow m r => Id DMOC.MerchantOperatingCity -> m ()
dropChain merchantOpCityId = do
  logInfo $ "SharedCabReconciler disabled for city " <> merchantOpCityId.getId <> "; dropping job chain"
  withTryCatch "sharedCabReconciler:releaseSeedMarker" (SharedCabSeed.releaseSharedCabReconcilerSeedMarker merchantOpCityId)
    >>= either (\err -> logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show err <> "; ops must DEL " <> SharedCabSeed.sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding") pure

-- | One keyset pass over the flagged drivers (resumes after the last driverId, so clearing a row cannot shift the walk).
reconcileAll :: ReconcileFlow m r => Id DMOC.MerchantOperatingCity -> Text -> BaseUrl -> Maybe (Id DP.Person) -> (Int, Int, Int) -> m (Int, Int, Int)
reconcileAll merchantOpCity apiKey internalUrl mbLastSeen (checked, cleared, leftFlag) = do
  flagged <- QDIExtra.findSharedCabActiveDriversAfter merchantOpCity mbLastSeen sharedCabReconcilerBatchSize
  case flagged of
    [] -> pure (checked, cleared, leftFlag)
    batch -> do
      (cleared', leftFlag') <- foldM (reconcileDriver apiKey internalUrl) (0 :: Int, 0 :: Int) batch
      reconcileAll merchantOpCity apiKey internalUrl ((.driverId) <$> lastMay batch) (checked + length batch, cleared + cleared', leftFlag + leftFlag')

reconcileDriver :: ReconcileFlow m r => Text -> BaseUrl -> (Int, Int) -> DDI.DriverInformation -> m (Int, Int)
reconcileDriver apiKey internalUrl (cleared, leftFlag) driverInfo = do
  let driverId = driverInfo.driverId
  mbVehicle <- QVeh.findById driverId
  case mbVehicle of
    Nothing -> do
      logError $ "SharedCabReconciler: driver " <> driverId.getId <> " flagged but has no vehicle; leaving flag set"
      pure (cleared, leftFlag + 1)
    Just vehicle -> do
      eProbe <- withTryCatch "sharedCabReconciler:probe" $ probeSharedCabSession apiKey internalUrl driverId.getId vehicle.registrationNo
      case eProbe of
        Right (SessionGone signal) -> do
          logWarning $ "SharedCabReconciler: clearing flag for driver " <> driverId.getId <> " on decoded signal " <> signal
          SharedCabFlag.clearSharedCabSessionActive driverId
          pure (cleared + 1, leftFlag)
        Right SessionUnknown -> do
          logDebug $ "SharedCabReconciler: no clear signal for driver " <> driverId.getId <> "; leaving flag"
          pure (cleared, leftFlag + 1)
        Left err -> do
          logWarning $ "SharedCabReconciler: probe failed for driver " <> driverId.getId <> ": " <> show err <> "; leaving flag"
          pure (cleared, leftFlag + 1)
