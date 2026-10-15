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
import EulerHS.Types (EulerClient, client)
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
import Servant hiding (throwError)
import SharedLogic.Allocator (AllocatorJobType (SharedCabReconciler), SharedCabReconcilerJobData (..))
import SharedLogic.CallBAPInternal (AppBackendBapInternal)
import qualified SharedLogic.SharedCab.Flag as SharedCabFlag
import qualified SharedLogic.SharedCab.ReconcilerSeed as SharedCabSeed
import Storage.Beam.SchedulerJob ()
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.DriverInformationExtra as QDIExtra
import qualified Storage.Queries.Vehicle as QVeh
import Tools.Metrics (CoreMetrics)

sharedCabReconcilerBatchSize :: Int
sharedCabReconcilerBatchSize = 100

clearSignalErrorCodes :: [Text]
clearSignalErrorCodes = ["SHARED_CAB_SESSION_NOT_FOUND", "SHARED_CAB_SESSION_HELD_BY_ANOTHER_DRIVER"]

type SharedCabSessionAPI =
  "internal"
    :> "sharedCab"
    :> "session"
    :> QueryParam' '[Required, Strict] "driverId" Text
    :> QueryParam' '[Required, Strict] "vehicleNumber" Text
    :> Header "token" Text
    :> Get '[JSON] Aeson.Value

callSessionClient :: Text -> Text -> Maybe Text -> EulerClient Aeson.Value
callSessionClient = client (Proxy @SharedCabSessionAPI)

callSessionAPI :: Proxy SharedCabSessionAPI
callSessionAPI = Proxy

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
      EC.callApiUnwrappingApiError APICallError Nothing (Just "BAP_INTERNAL_API_ERROR") (Just internalEndPointHashMap) internalUrl (callSessionClient driverId vehicleNumber (Just apiKey)) "GetSharedCabSession" callSessionAPI
  pure $ case eResp of
    Right _ -> SessionUnknown -- 2xx: a live session answered; nothing to do
    Left exc
      | Just (APICallError apiErr) <- fromException @APICallError exc,
        apiErr.errorCode `elem` clearSignalErrorCodes ->
        SessionGone apiErr.errorCode
      | otherwise -> SessionUnknown -- everything else: fail-closed, keep the flag

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
  -- Heartbeat BEFORE the enabled check, so even a disabled fire refreshes the seed marker. Best-effort: must not kill the chain.
  eRefresh <- withTryCatch "sharedCabReconciler:refreshSeedMarker" $ SharedCabSeed.refreshSharedCabReconcilerSeedMarker merchantOpCityId
  case eRefresh of
    Right () -> pure ()
    Left err -> logError $ "SharedCabReconciler: seed marker refresh failed for city " <> merchantOpCityId.getId <> ": " <> show err
  mbTc <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing
  let reconcilerOn = maybe False (fromMaybe False . (.sharedCabReconcilerEnabled)) mbTc
  if not reconcilerOn
    then do
      logInfo $ "SharedCabReconciler disabled for city " <> merchantOpCityId.getId <> "; dropping job chain"
      -- The chain is intentionally ending: release the seed marker. Best-effort: a DEL failure must not throw; log the key for manual cleanup.
      eRelease <- withTryCatch "sharedCabReconciler:releaseSeedMarker" $ SharedCabSeed.releaseSharedCabReconcilerSeedMarker merchantOpCityId
      case eRelease of
        Right () -> pure ()
        Left err ->
          logError $ "SharedCabReconciler: failed to release seed marker for city " <> merchantOpCityId.getId <> ": " <> show err <> "; ops must DEL " <> SharedCabSeed.sharedCabReconcilerSeededKey merchantOpCityId <> " (cross-app key) before reseeding"
      pure Complete
    else do
      bap <- asks (.appBackendBapInternal)
      eSweep <- withTryCatch "sharedCabReconciler:sweep" $ reconcileAll merchantOpCityId bap.apiKey bap.url Nothing (0 :: Int, 0 :: Int, 0 :: Int)
      case eSweep of
        Right (checked, cleared, leftFlag) ->
          logInfo $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " checked=" <> show (checked :: Int) <> " cleared=" <> show (cleared :: Int) <> " left=" <> show (leftFlag :: Int)
        Left err ->
          logError $ "SharedCabReconciler city=" <> merchantOpCityId.getId <> " sweep aborted: " <> show err <> "; re-enqueueing, un-reconciled flags stay as they were"
      JC.createJobIn @_ @'SharedCabReconciler (Just merchantId) (Just merchantOpCityId) SharedCabSeed.sharedCabReconcilerInterval jobData
      pure Complete
  where
    -- One keyset pass over the flagged drivers (resumes after the last driverId, so clearing a row cannot shift the walk).
    reconcileAll :: Id DMOC.MerchantOperatingCity -> Text -> BaseUrl -> Maybe (Id DP.Person) -> (Int, Int, Int) -> m (Int, Int, Int)
    reconcileAll merchantOpCity apiKey internalUrl mbLastSeen (checked, cleared, leftFlag) = do
      flagged <- QDIExtra.findSharedCabActiveDriversAfter merchantOpCity mbLastSeen sharedCabReconcilerBatchSize
      case flagged of
        [] -> pure (checked, cleared, leftFlag)
        batch -> do
          (cleared', leftFlag') <- foldM (reconcileDriver apiKey internalUrl) (0 :: Int, 0 :: Int) batch
          reconcileAll merchantOpCity apiKey internalUrl (Just (last batch).driverId) (checked + length batch, cleared + cleared', leftFlag + leftFlag')

    reconcileDriver :: Text -> BaseUrl -> (Int, Int) -> DDI.DriverInformation -> m (Int, Int)
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
              clearFlag driverId
              pure (cleared + 1, leftFlag)
            Right SessionUnknown -> do
              logDebug $ "SharedCabReconciler: no clear signal for driver " <> driverId.getId <> "; leaving flag"
              pure (cleared, leftFlag + 1)
            Left err -> do
              logWarning $ "SharedCabReconciler: probe failed for driver " <> driverId.getId <> ": " <> show err <> "; leaving flag"
              pure (cleared, leftFlag + 1)

    clearFlag :: Id DP.Person -> m ()
    clearFlag driverId = SharedCabFlag.clearSharedCabSessionActive driverId
