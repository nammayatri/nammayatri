module SharedLogic.DriverOnboarding.OnboardingFlags.Guard
  ( module SharedLogic.DriverOnboarding.OnboardingFlags.Checks,
    defaultRecomputeSpec,
    runRecomputeSpec,
    withOnboardingAction,
    withOnboardingActionFanout,
  )
where

import Data.List (nub)
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified SharedLogic.Association.Change as AC
import SharedLogic.DriverOnboarding.OnboardingFlags.Checks
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery

defaultRecomputeSpec :: GuardTarget -> RecomputeSpec
defaultRecomputeSpec = \case
  TargetDriver personId -> recomputeDrivers [personId]
  TargetFleetOwner personId -> recomputeFleetOwners [personId]
  TargetVehicle registrationNo -> recomputeVehicles [registrationNo]
  TargetVehicleById rcId -> mempty {rsVehicleIds = [rcId]}

runRecomputeSpec :: OnboardingFlow m r => DTC.TransporterConfig -> RecomputeSpec -> m ()
runRecomputeSpec transporterConfig spec = do
  forM_ (nub spec.rsDrivers) $ \personId ->
    void $ SStatus.runRefreshOnboardingFlagsDriver Nothing (Just transporterConfig) personId
  forM_ (nub spec.rsFleetOwners) $ \personId ->
    void $ SStatus.runRefreshOnboardingFlagsFleet Nothing (Just transporterConfig) personId
  forM_ (nub spec.rsVehicleRegNos) $ \registrationNo -> do
    mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
    whenJust mbRc $ \rc -> void $ SStatus.runRefreshOnboardingFlagsVehicle (Just transporterConfig) rc.id
  forM_ (nub spec.rsVehicleIds) $ \rcId ->
    void $ SStatus.runRefreshOnboardingFlagsVehicle (Just transporterConfig) rcId

withOnboardingAction :: OnboardingFlow m r => DTC.TransporterConfig -> Actor -> ActionVerb -> GuardTarget -> m a -> m a
withOnboardingAction transporterConfig actor verb target body =
  withOnboardingActionFanout transporterConfig actor verb target ((,mempty) <$> body)

withOnboardingActionFanout :: OnboardingFlow m r => DTC.TransporterConfig -> Actor -> ActionVerb -> GuardTarget -> m (a, RecomputeSpec) -> m a
withOnboardingActionFanout transporterConfig actor verb target body =
  withOnboardingActionLock target $
    AC.withAssociation (guardOnboardingAction transporterConfig actor verb target) $ do
      (result, extraSpec) <- body
      when (isUnified transporterConfig) $
        runRecomputeSpec transporterConfig (defaultRecomputeSpec target <> extraSpec)
      pure result

onboardingActionLockTTLSeconds :: Int
onboardingActionLockTTLSeconds = 30

onboardingActionLockRetryMs :: Int
onboardingActionLockRetryMs = 100

-- | Serialise concurrent onboarding actions on the same entity: two handlers mutating the same
--   driver would otherwise interleave their guard read, their write and their recompute, and the
--   later recompute could observe a half-applied state. Waits for the holder rather than failing.
withOnboardingActionLock :: OnboardingFlow m r => GuardTarget -> m a -> m a
withOnboardingActionLock target body = case target of
  TargetDriver personId -> locked personId.getId
  TargetFleetOwner personId -> locked personId.getId
  TargetVehicleById rcId -> locked rcId.getId
  TargetVehicle registrationNo -> locked registrationNo
  where
    locked entityKey =
      Hedis.withWaitAndLockRedis ("Onboarding:Action:" <> entityKey) onboardingActionLockTTLSeconds onboardingActionLockRetryMs body
