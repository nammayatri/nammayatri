module SharedLogic.DriverOnboarding.OnboardingFlags.Guard
  ( module SharedLogic.DriverOnboarding.OnboardingFlags.Checks,
    defaultRecomputeSpec,
    runRecomputeSpec,
    withOnboardingAction,
    withOnboardingActionFanout,
  )
where

import Data.List (nub)
import qualified Domain.Types.Alert.AlertEntityType as DAlertEntity
import qualified Domain.Types.Alert.OnboardingAlertAction as DOnboardingAlertAction
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common (fork)
import qualified SharedLogic.DashboardAlert as SDA
import SharedLogic.DriverOnboarding.OnboardingFlags.Checks
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import Storage.Beam.DashboardAlert ()
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery

defaultRecomputeSpec :: GuardTarget -> RecomputeSpec
defaultRecomputeSpec = \case
  TargetDriver personId -> recomputeDrivers [personId]
  TargetDriverVehicle personId registrationNo -> recomputeDrivers [personId] <> recomputeVehicles [registrationNo]
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
  withOnboardingActionLock target $ do
    guardOnboardingAction transporterConfig actor verb target
    (result, extraSpec) <- body
    when (isUnified transporterConfig) $ do
      runRecomputeSpec transporterConfig (defaultRecomputeSpec target <> extraSpec)
      when (verb `elem` notifiableVerbs) $
        fork "Dashboard onboarding alert" (notifyOnboardingAction actor verb target)
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
  TargetDriverVehicle personId _ -> locked personId.getId
  TargetFleetOwner personId -> locked personId.getId
  TargetVehicleById rcId -> locked rcId.getId
  TargetVehicle registrationNo -> locked registrationNo
  where
    locked entityKey =
      Hedis.withWaitAndLockRedis ("Onboarding:Action:" <> entityKey) onboardingActionLockTTLSeconds onboardingActionLockRetryMs body

fleetOnlyNotifiableVerbs :: [ActionVerb]
fleetOnlyNotifiableVerbs =
  [ UnlinkVehicle,
    ActivateVehicle,
    DeactivateVehicle,
    UnlinkFromFleet,
    ActivateToFleet,
    DeactivateFromFleet
  ]

notifiableVerbs :: [ActionVerb]
notifiableVerbs =
  fleetOnlyNotifiableVerbs
    <> [ Delete,
         ChangeFleetOwner
       ]

targetEntity :: GuardTarget -> (DAlertEntity.AlertEntityType, Text, Maybe (Id DP.Person))
targetEntity = \case
  TargetDriver personId -> (DAlertEntity.DriverEntity, personId.getId, Just personId)
  TargetDriverVehicle personId registrationNo -> (DAlertEntity.VehicleEntity, registrationNo, Just personId)
  TargetFleetOwner personId -> (DAlertEntity.FleetEntity, personId.getId, Nothing)
  TargetVehicle registrationNo -> (DAlertEntity.VehicleEntity, registrationNo, Nothing)
  TargetVehicleById rcId -> (DAlertEntity.VehicleEntity, rcId.getId, Nothing)

actorFleetOwner :: Actor -> Maybe (Id DP.Person)
actorFleetOwner = \case
  ActorFleet fleetOwnerId -> Just fleetOwnerId
  ActorFleetAndDriver fleetOwnerId _ -> Just fleetOwnerId
  ActorDriver _ -> Nothing
  None -> Nothing

notifyOnboardingAction :: OnboardingFlow m r => Actor -> ActionVerb -> GuardTarget -> m ()
notifyOnboardingAction actor verb target = do
  let (entityType, entityId, mbDriverId) = targetEntity target
  mbTargetFleetOwner <- case mbDriverId of
    Just driverId -> fmap (Id . (.fleetOwnerId)) <$> QFDA.findByDriverId driverId True
    Nothing -> pure Nothing
  let alertActor = maybe SDA.SystemActor SDA.FleetOwnerActor (actorFleetOwner actor)
      fleetOwnerIds = nub $ catMaybes [actorFleetOwner actor, mbTargetFleetOwner]
      allAudiences = SDA.audiencesFor alertActor fleetOwnerIds
      audiences =
        if verb `elem` fleetOnlyNotifiableVerbs
          then SDA.withoutAdminAudience allAudiences
          else allAudiences
      requestorId = fromMaybe (Id "system") (actorPersonId actor)
  mbPerson <- QPerson.findById requestorId
  whenJust mbPerson $ \person -> do
    let dynamicParams =
          [ ("entityId", entityId),
            ("entityType", show entityType),
            ("action", show verb),
            ("requestorName", person.firstName <> maybe "" (" " <>) person.lastName)
          ]
    SDA.notifyOnboardingChange
      audiences
      entityType
      entityId
      (castActionVerb verb)
      dynamicParams
      requestorId
      person.merchantId
      person.merchantOperatingCityId

actorPersonId :: Actor -> Maybe (Id DP.Person)
actorPersonId = \case
  ActorFleet personId -> Just personId
  ActorDriver personId -> Just personId
  ActorFleetAndDriver _ personId -> Just personId
  None -> Nothing

castActionVerb :: ActionVerb -> DOnboardingAlertAction.OnboardingAlertAction
castActionVerb = \case
  LinkVehicle -> DOnboardingAlertAction.LinkVehicleAction
  UnlinkVehicle -> DOnboardingAlertAction.UnlinkVehicleAction
  ActivateVehicle -> DOnboardingAlertAction.ActivateVehicleAction
  DeactivateVehicle -> DOnboardingAlertAction.DeactivateVehicleAction
  LinkToOperator -> DOnboardingAlertAction.LinkToOperatorAction
  UnlinkFromOperator -> DOnboardingAlertAction.UnlinkFromOperatorAction
  UnlinkFromFleet -> DOnboardingAlertAction.UnlinkFromFleetAction
  Add -> DOnboardingAlertAction.AddAction
  Delete -> DOnboardingAlertAction.DeleteAction
  Enable -> DOnboardingAlertAction.EnableAction
  Disable -> DOnboardingAlertAction.DisableAction
  Block -> DOnboardingAlertAction.BlockAction
  Unblock -> DOnboardingAlertAction.UnblockAction
  Approve -> DOnboardingAlertAction.ApproveAction
  Reject -> DOnboardingAlertAction.RejectAction
  SetOnboardingAs -> DOnboardingAlertAction.SetOnboardingAsAction
  LinkToFleet -> DOnboardingAlertAction.LinkToFleetAction
  ActivateToFleet -> DOnboardingAlertAction.ActivateToFleetAction
  DeactivateFromFleet -> DOnboardingAlertAction.DeactivateFromFleetAction
  View -> DOnboardingAlertAction.ViewAction
  ChangeFleetOwner -> DOnboardingAlertAction.ChangeFleetOwnerAction
  Expire -> DOnboardingAlertAction.ExpireAction
  UnlinkDocument -> DOnboardingAlertAction.UnlinkDocumentAction
  OnboardingFlagMutation -> DOnboardingAlertAction.OnboardingFlagMutationAction
