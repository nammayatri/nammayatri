module SharedLogic.Scheduler.Jobs.SharedCabDemandNudge
  ( sharedCabDemandNudge,
  )
where

import qualified BecknV2.OnDemand.Enums as Enums
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import SharedLogic.JobScheduler
import SharedLogic.SharedCab.Allocation (InternalEndpointFlow)
import qualified SharedLogic.SharedCab.Config as Config
import SharedLogic.SharedCab.DemandNudge (runDemandNudge)
import SharedLogic.SharedCab.Enabled (sharedCabEnabled)
import SharedLogic.SharedCab.LegState (isSharedCabAgency)
import SharedLogic.SharedCab.Schedule (claimDemandNudgeRun, claimedTick, ownDemandNudgeChain, scheduleNextDemandNudge)
import SharedLogic.SharedCab.SessionState (SessionFlow)
import Storage.Beam.SchedulerJob ()
import qualified Storage.CachedQueries.Merchant as CQM
import Tools.Error

type DemandNudgeFlow m r c = (SessionFlow m r c, MonadMask m, JobCreator r m, Redis.HedisLTSFlowEnv r, Metrics.CoreMetrics m, HasShortDurationRetryCfg r c, InternalEndpointFlow m r, HasRequestId r)

sharedCabDemandNudge :: DemandNudgeFlow m r c => Job 'SharedCabDemandNudge -> m ExecutionResult
sharedCabDemandNudge Job {jobInfo} = do
  let jobData = jobInfo.jobData
      mocId = jobData.merchantOperatingCityId
  cfg <- Config.getTunables mocId
  ownDemandNudgeChain jobData.merchantId mocId cfg.nudgeIntervalSec jobData.chainId $ \chainId -> do
    continueRef <- liftIO (newIORef True)
    claimedTick
      (claimDemandNudgeRun mocId cfg.nudgeIntervalSec)
      (nudgeCity cfg jobData.merchantId mocId >>= liftIO . writeIORef continueRef)
      (liftIO (readIORef continueRef) >>= \continue -> when continue (scheduleNextDemandNudge jobData.merchantId mocId cfg.nudgeIntervalSec chainId))
  pure Complete

nudgeCity :: DemandNudgeFlow m r c => Config.SharedCabTunables -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m Bool
nudgeCity cfg merchantId mocId = do
  enabled <- sharedCabEnabled mocId
  mbIbc <- find (isSharedCabAgency . (.agencyKey)) <$> SIBC.findAllIntegratedBPPConfig mocId Enums.BUS DIBC.APPLICATION
  case mbIbc of
    Just ibc | enabled && cfg.demandNudgeEnabled -> do
      (active, routes, plates) <- runDemandNudge cfg ibc
      unless (null routes) $
        withTryCatch "sharedCab:demandNudgeSend" (send routes plates) >>= \case
          Left err -> logError $ "shared-cab demand nudge: driver-app call failed: " <> show err
          Right _ -> pure ()
      pure active
    _ -> pure False
  where
    send routes plates = do
      merchant <- CQM.findById merchantId >>= fromMaybeM (MerchantNotFound merchantId.getId)
      now <- getCurrentTime
      CallBPPInternal.sharedCabDemandNudge merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId $
        CallBPPInternal.SharedCabDemandNudgeReq
          { routes,
            onRoutePlates = plates,
            tunables =
              CallBPPInternal.NudgeTunables
                { minRiders = cfg.nudgeMinRiders,
                  maxLookups = cfg.nudgeMaxLookupsPerCity,
                  idleRadiusM = cfg.nudgeIdleRadiusM,
                  mergeRadiusM = cfg.nudgeMergeRadiusM,
                  maxOptions = cfg.nudgeMaxOptionsPerDriver,
                  maxIdlePerRoute = cfg.nudgeMaxIdleDriversPerRoute,
                  expiresAt = addUTCTime (fromIntegral cfg.nudgeIntervalSec) now
                }
          }
