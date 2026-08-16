module SharedLogic.DriverOnboarding.OnboardingFlags.Guard
  ( ActionVerb (..),
    Actor (..),
    GuardTarget (..),
    RecomputeSpec (..),
    EntitySnapshot (..),
    GuardViolation (..),
    guardOnboardingAction,
    withOnboardingAction,
  )
where

import Data.List (nub)
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Association.Change as AC
import SharedLogic.DriverOnboarding.Common (hasActiveFleetAssociation)
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOA
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetRCAssociation as FRCA
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error

data ActionVerb
  = Link
  | Unlink
  | Activate
  | Deactivate
  | Add
  | Delete
  | Enable
  | Disable
  | Block
  | Unblock
  | Approve
  | Reject
  | SetOnboardingAs
  | LinkToFleet
  | View
  | ChangeFleetOwner
  | Expire
  deriving (Show, Eq, Generic)

-- | Who the action runs on behalf of. Every post-onboarding action -- one performed on an entity
--   that is already enabled -- names its actor, so the actor's own flags can gate it. 'None' is
--   reserved for the onboarding and registration stages, where the flags are still being derived
--   and there is no enabled actor to hold to account yet.
data Actor
  = ActorFleet (Id DP.Person)
  | ActorDriver (Id DP.Person)
  | -- | Fleet owner and driver both act: both sets of flags have to pass.
    ActorFleetAndDriver (Id DP.Person) (Id DP.Person)
  | None
  deriving (Show, Eq, Generic)

data GuardTarget
  = TargetDriver (Id DP.Person)
  | TargetVehicle Text
  | TargetVehicleById (Id DVRC.VehicleRegistrationCertificate)
  | TargetFleetOwner (Id DP.Person)
  deriving (Show, Eq, Generic)

data GuardViolation = GuardViolation
  { gvInvariant :: Text,
    gvDetail :: Text
  }
  deriving (Show, Generic)

data EntitySnapshot
  = SnapDriver DI.DriverInformation Bool Bool
  | SnapVehicle DVRC.VehicleRegistrationCertificate
  | SnapFleet DFOI.FleetOwnerInformation

data RecomputeSpec = RecomputeSpec
  { rsDrivers :: [Id DP.Person],
    rsFleetOwners :: [Id DP.Person],
    rsVehicleRegNos :: [Text],
    rsVehicleIds :: [Id DVRC.VehicleRegistrationCertificate]
  }
  deriving (Show, Generic)

instance Semigroup RecomputeSpec where
  a <> b =
    RecomputeSpec
      { rsDrivers = a.rsDrivers <> b.rsDrivers,
        rsFleetOwners = a.rsFleetOwners <> b.rsFleetOwners,
        rsVehicleRegNos = a.rsVehicleRegNos <> b.rsVehicleRegNos,
        rsVehicleIds = a.rsVehicleIds <> b.rsVehicleIds
      }

instance Monoid RecomputeSpec where
  mempty = RecomputeSpec [] [] [] []

recomputeDrivers :: [Id DP.Person] -> RecomputeSpec
recomputeDrivers ids = mempty {rsDrivers = ids}

recomputeFleetOwners :: [Id DP.Person] -> RecomputeSpec
recomputeFleetOwners ids = mempty {rsFleetOwners = ids}

recomputeVehicles :: [Text] -> RecomputeSpec
recomputeVehicles regNos = mempty {rsVehicleRegNos = regNos}

checkPrecondition :: ActionVerb -> EntitySnapshot -> Either GuardViolation ()
checkPrecondition verb = \case
  SnapDriver driverInfo hasFleetAssoc hasRcAssoc -> checkDriver verb driverInfo hasFleetAssoc hasRcAssoc
  SnapVehicle rc -> checkVehicle verb rc
  SnapFleet fleetInfo -> checkFleet verb fleetInfo

checkDriver :: ActionVerb -> DI.DriverInformation -> Bool -> Bool -> Either GuardViolation ()
checkDriver verb driverInfo _hasFleetAssoc _hasRcAssoc = case verb of
  Enable
    | isNothing driverInfo.disabledReasonFlag ->
      violate "DI-3" "driver is not administratively disabled; enablement is derived from documents"
    | driverInfo.disabledReasonFlag == Just DI.FleetDisabled ->
      violate "DI-6" "driver is disabled because its fleet is disabled; enable the fleet instead"
    | not driverInfo.verified ->
      violate "DI-2" "driver is not verified; enablement is derived from documents"
    | driverInfo.approved /= Just True ->
      violate "DI-2" "driver is not approved; enablement is derived from documents"
    | not driverInfo.enabled ->
      violate "DI-2" "driver is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Disable
    | isJust driverInfo.disabledReasonFlag -> violate "DI-3" "driver is already disabled"
    | not driverInfo.verified ->
      violate "DI-2" "driver is not verified; enablement is derived from documents"
    | driverInfo.approved /= Just True ->
      violate "DI-2" "driver is not approved; enablement is derived from documents"
    | not driverInfo.enabled ->
      violate "DI-2" "driver is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Link
    | not driverInfo.enabled -> violate "DI-1" "driver is not enabled"
    | driverInfo.blocked -> violate "D-BLOCKED" "driver is blocked"
    | driverInfo.approved /= Just True -> violate "DI-1" "driver is not approved"
    | otherwise -> ok
  Activate
    | not driverInfo.enabled -> violate "DI-1" "driver is not enabled"
    | driverInfo.blocked -> violate "D-BLOCKED" "driver is blocked"
    | otherwise -> ok
  Delete
    | isNothing driverInfo.disabledReasonFlag -> violate "D-DELETE" "driver is not administratively disabled; disable before deleting"
    | otherwise -> ok
  Unblock
    | not driverInfo.blocked -> violate "D-BLOCKED" "driver is not blocked"
    | isNothing driverInfo.blockReasonFlag -> violate "D-BLOCKED" "driver has no block reason flag"
    | not driverInfo.verified ->
      violate "DI-2" "driver is not verified; enablement is derived from documents"
    | driverInfo.approved /= Just True ->
      violate "DI-2" "driver is not approved; enablement is derived from documents"
    | not driverInfo.enabled ->
      violate "DI-2" "driver is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Block
    | driverInfo.blocked -> violate "D-BLOCKED" "driver is already blocked"
    | isJust driverInfo.blockReasonFlag -> violate "D-BLOCKED" "driver already has a block reason flag"
    | not driverInfo.verified ->
      violate "DI-2" "driver is not verified; enablement is derived from documents"
    | driverInfo.approved /= Just True ->
      violate "DI-2" "driver is not approved; enablement is derived from documents"
    | not driverInfo.enabled ->
      violate "DI-2" "driver is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Unlink -> ok
  Deactivate -> ok
  Add -> ok
  Approve -> ok
  Reject -> ok
  SetOnboardingAs
    | driverInfo.enabled -> violate "DI-8" "onboardingAs cannot be changed while the driver is enabled"
    | otherwise -> ok
  LinkToFleet
    | driverInfo.enabled -> violate "DI-9" "driver is already enabled; use changeFleetOwner to move an active driver between fleets"
    | otherwise -> ok
  View -> ok
  ChangeFleetOwner -> ok
  Expire -> ok

checkVehicle :: ActionVerb -> DVRC.VehicleRegistrationCertificate -> Either GuardViolation ()
checkVehicle verb rc = case verb of
  Link
    | rc.verificationStatus /= Documents.VALID -> violate "RI-2" "RC verification status is not VALID"
    | rc.verified /= Just True -> violate "RI-2" "RC is not verified"
    | rc.approved /= Just True -> violate "RI-1" "RC is not approved"
    | otherwise -> ok
  Activate
    | rc.verificationStatus /= Documents.VALID -> violate "RI-2" "RC verification status is not VALID"
    | rc.approved /= Just True -> violate "RI-1" "RC is not approved"
    | otherwise -> ok
  Enable -> violate "R-UNSUPPORTED" "vehicles have no enabled flag"
  Disable -> violate "R-UNSUPPORTED" "vehicles have no enabled flag"
  Block -> violate "R-UNSUPPORTED" "vehicles have no blocked flag"
  Unblock -> violate "R-UNSUPPORTED" "vehicles have no blocked flag"
  Unlink -> ok
  Deactivate -> ok
  Add -> ok
  Delete -> ok
  Approve -> ok
  Reject -> ok
  SetOnboardingAs -> violate "R-UNSUPPORTED" "vehicles have no onboardingAs"
  LinkToFleet -> violate "R-UNSUPPORTED" "vehicles are not linked to fleets by this verb"
  View -> ok
  ChangeFleetOwner -> violate "R-UNSUPPORTED" "vehicles do not change fleet owner by this verb"
  Expire -> ok

checkFleet :: ActionVerb -> DFOI.FleetOwnerInformation -> Either GuardViolation ()
checkFleet verb fleetInfo = case verb of
  Enable
    | isNothing fleetInfo.disabledReasonFlag ->
      violate "FI-1" "fleet owner is not administratively disabled; enablement is derived from documents"
    | not fleetInfo.verified ->
      violate "FI-2" "fleet owner is not verified; enablement is derived from documents"
    | fleetInfo.approved /= Just True ->
      violate "FI-2" "fleet owner is not approved; enablement is derived from documents"
    | not fleetInfo.enabled ->
      violate "FI-2" "fleet owner is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Disable
    | isJust fleetInfo.disabledReasonFlag -> violate "FI-1" "fleet owner is already disabled"
    | not fleetInfo.verified ->
      violate "FI-2" "fleet owner is not verified; enablement is derived from documents"
    | fleetInfo.approved /= Just True ->
      violate "FI-2" "fleet owner is not approved; enablement is derived from documents"
    | not fleetInfo.enabled ->
      violate "FI-2" "fleet owner is not enabled; enablement is derived from documents"
    | otherwise -> ok
  Block -> violate "FI-3" "fleet owner block is not supported; use disable"
  Unblock -> violate "FI-3" "fleet owner unblock is not supported; use enable"
  Link -> ok
  Add -> ok
  Activate -> ok
  Deactivate -> ok
  Unlink -> ok
  Delete -> ok
  Approve -> ok
  Reject -> ok
  SetOnboardingAs -> violate "F-UNSUPPORTED" "fleet owners have no onboardingAs"
  LinkToFleet -> ok
  View -> ok
  ChangeFleetOwner -> violate "F-UNSUPPORTED" "fleet owners do not change fleet owner"
  Expire -> ok

-- | The actor is the person performing the action, as opposed to the entity it is performed on.
--   A fleet owner or driver that is itself disabled or blocked may not mutate the drivers and
--   vehicles under it. Roles that carry no onboarding flags (admin, operator) are unconstrained.
checkActorFleet :: DFOI.FleetOwnerInformation -> Either GuardViolation ()
checkActorFleet fleetInfo
  | fleetInfo.blocked = violate "ACTOR-2" "acting fleet owner is blocked"
  | isJust fleetInfo.disabledReasonFlag = violate "ACTOR-3" "acting fleet owner is disabled"
  | not fleetInfo.enabled = violate "ACTOR-1" "acting fleet owner is not enabled"
  | otherwise = ok

checkActorDriver :: DI.DriverInformation -> Either GuardViolation ()
checkActorDriver driverInfo
  | driverInfo.blocked = violate "ACTOR-2" "acting driver is blocked"
  | isJust driverInfo.disabledReasonFlag = violate "ACTOR-3" "acting driver is disabled"
  | not driverInfo.enabled = violate "ACTOR-1" "acting driver is not enabled"
  | otherwise = ok

ok :: Either GuardViolation ()
ok = Right ()

violate :: Text -> Text -> Either GuardViolation ()
violate invariant detail = Left $ GuardViolation invariant detail

isUnified :: DTC.TransporterConfig -> Bool
isUnified transporterConfig = transporterConfig.unifiedOnboardingFlagsRecompute == Just True

loadSnapshot :: OnboardingFlow m r => GuardTarget -> m (Maybe EntitySnapshot)
loadSnapshot = \case
  TargetDriver personId -> do
    mbDriverInfo <- DIQuery.findById (cast personId)
    case mbDriverInfo of
      Nothing -> pure Nothing
      Just driverInfo -> do
        hasFleetAssoc <- hasActiveFleetAssociation personId
        hasRcAssoc <- isJust <$> DRAQuery.findActiveAssociationByDriver (cast personId) True
        pure $ Just $ SnapDriver driverInfo hasFleetAssoc hasRcAssoc
  TargetFleetOwner personId -> fmap SnapFleet <$> QFOI.findByPrimaryKey personId
  TargetVehicle registrationNo -> fmap SnapVehicle <$> RCQuery.findLastVehicleRCWrapper registrationNo
  TargetVehicleById rcId -> fmap SnapVehicle <$> RCQuery.findById rcId

reportViolation :: OnboardingFlow m r => ActionVerb -> GuardTarget -> GuardViolation -> m ()
reportViolation verb _target violation =
  throwError $
    InvalidRequest $
      show verb <> " not allowed: " <> violation.gvDetail <> " [" <> violation.gvInvariant <> "]"

-- | Effectful preconditions that cannot live in the pure core: they need a ride lookup.
--   Scoped by target, so an RC-scoped action resolves live rides through the RC's active driver
--   and a fleet-scoped one across every active driver in the fleet.
guardNoLiveRide :: OnboardingFlow m r => ActionVerb -> GuardTarget -> m ()
guardNoLiveRide verb target
  | verb `notElem` [Unlink, Deactivate, Delete, Disable, Expire] = pure ()
  | otherwise = case target of
    TargetDriver personId -> AC.guardNoLiveRideByDriver personId
    TargetVehicle registrationNo -> do
      mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
      whenJust mbRc $ \rc -> AC.guardNoLiveRideByRC rc.id
    TargetVehicleById rcId -> AC.guardNoLiveRideByRC rcId
    TargetFleetOwner personId -> AC.guardNoLiveRideInFleet personId.getId

-- | The association guards apply to every city: a live ride blocks an association change whether
--   or not the city derives its flags. Only the flag-state preconditions are unified-only.
-- | A driver may only be linked into a new fleet or operator when they hold no live association,
--   unless the merchant opts into overwriting. Resolved from driverInfo.merchantId so the guard
--   keeps its TransporterConfig-only signature.
guardAssociationAllowed :: OnboardingFlow m r => ActionVerb -> GuardTarget -> m ()
guardAssociationAllowed verb target
  | verb `notElem` [Link, Add, LinkToFleet] = pure ()
  | otherwise = case target of
    TargetDriver personId -> do
      mbDriverInfo <- DIQuery.findById (cast personId)
      whenJust (mbDriverInfo >>= (.merchantId)) $ \merchantId -> do
        mbMerchant <- CQM.findById merchantId
        whenJust mbMerchant $ \merchant ->
          unless (merchant.overwriteAssociation == Just True) $ do
            hasFleetAssoc <- hasActiveFleetAssociation personId
            when hasFleetAssoc $ throwError (InvalidRequest "Driver already associated with a fleet")
            existingOperatorAssocs <- QDOA.findAllByDriverId personId True
            unless (null existingOperatorAssocs) $
              throwError (InvalidRequest "Driver is already associated with an operator")
    _ -> pure ()

-- | RC-side association guards, previously inside the creation helpers.
guardRcAssociationAllowed :: OnboardingFlow m r => DTC.TransporterConfig -> ActionVerb -> GuardTarget -> m ()
guardRcAssociationAllowed transporterConfig verb target
  | verb `notElem` [Link, Add, Activate, LinkToFleet] = pure ()
  | transporterConfig.blockDriverOwnRCForFleetDrivers /= Just True = pure ()
  | otherwise = case target of
    TargetVehicleById rcId -> AC.guardRCNotActiveWithAnotherDriver rcId
    TargetVehicle registrationNo -> do
      mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
      whenJust mbRc $ \rc -> AC.guardRCNotActiveWithAnotherDriver rc.id
    _ -> pure ()

-- | Driver- and vehicle-scoped actions are the ones performed on behalf of a fleet owner or
--   driver; a fleet-owner-scoped action is an administrative one on the fleet itself, so the
--   actor's own flags do not gate it.
isActorGovernedTarget :: GuardTarget -> Bool
isActorGovernedTarget = \case
  TargetDriver _ -> True
  TargetVehicle _ -> True
  TargetVehicleById _ -> True
  TargetFleetOwner _ -> False

-- | Which half of the actor a verb holds to account. A fleet owner answers for everything it
--   initiates, so it is checked for every verb. The acting driver is only answerable for verbs that
--   put them or their vehicle into service: teardown (unlink, deactivate, delete, reject) and the
--   onboarding verbs have to stay available while the driver is still disabled, or a disabled driver
--   could never be cleaned up -- nor onboarded, since they are disabled until their documents land.
--   So the call site names both actors whenever both exist, and this decides what that means.
driverActorGoverned :: ActionVerb -> Bool
driverActorGoverned verb = verb `elem` [Link, Activate, Add]

guardActorAllowed :: OnboardingFlow m r => ActionVerb -> GuardTarget -> Actor -> m ()
guardActorAllowed verb target actor
  | verb == ChangeFleetOwner = pure ()
  | not (isActorGovernedTarget target) = pure ()
  | otherwise = case actor of
    None -> pure ()
    ActorFleet fleetOwnerId -> checkFleetActor fleetOwnerId
    ActorDriver driverPersonId -> when (driverActorGoverned verb) $ checkDriverActor driverPersonId
    ActorFleetAndDriver fleetOwnerId driverPersonId -> do
      checkFleetActor fleetOwnerId
      when (driverActorGoverned verb) $ checkDriverActor driverPersonId
  where
    checkFleetActor fleetOwnerId = do
      mbFleetInfo <- QFOI.findByPrimaryKey fleetOwnerId
      whenJust mbFleetInfo $ \fleetInfo -> report (checkActorFleet fleetInfo)
    checkDriverActor driverPersonId = do
      mbDriverInfo <- DIQuery.findById (cast driverPersonId)
      whenJust mbDriverInfo $ \driverInfo -> report (checkActorDriver driverInfo)
    report = \case
      Right () -> pure ()
      Left violation -> reportViolation verb target violation

guardActorScope :: OnboardingFlow m r => ActionVerb -> GuardTarget -> Actor -> m ()
guardActorScope verb target actor
  | verb /= View = pure ()
  | otherwise = case actor of
    None -> pure ()
    ActorDriver _ -> pure ()
    ActorFleet fleetOwnerId -> checkFleetScope fleetOwnerId
    ActorFleetAndDriver fleetOwnerId _ -> checkFleetScope fleetOwnerId
  where
    checkFleetScope fleetOwnerId = case target of
      TargetDriver personId -> do
        mbAssoc <- QFDA.findByDriverIdAndFleetOwnerIdWithStatus personId fleetOwnerId.getId
        when (isNothing mbAssoc) $ throwError DriverNotPartOfFleet
      TargetVehicle registrationNo -> do
        mbRc <- RCQuery.findLastVehicleRCFleet' registrationNo fleetOwnerId.getId
        when (isNothing mbRc) $ throwError VehicleNotPartOfFleet
      TargetVehicleById rcId -> do
        mbRc <- RCQuery.findById rcId
        unless (((.fleetOwnerId) =<< mbRc) == Just fleetOwnerId.getId) $ throwError VehicleNotPartOfFleet
      TargetFleetOwner otherFleetOwnerId ->
        unless (otherFleetOwnerId == fleetOwnerId) $ throwError (InvalidFleetOwner otherFleetOwnerId.getId)

guardFleetVehicleRelations :: OnboardingFlow m r => ActionVerb -> GuardTarget -> Actor -> m ()
guardFleetVehicleRelations verb target actor
  | verb `notElem` [Link, Unlink] = pure ()
  | otherwise = case target of
    TargetVehicleById rcId -> checkVehicleTarget (pure $ Just rcId)
    TargetVehicle registrationNo -> checkVehicleTarget (fmap (.id) <$> RCQuery.findLastVehicleRCWrapper registrationNo)
    _ -> pure ()
  where
    checkVehicleTarget resolveRcId = case actor of
      ActorFleetAndDriver fleetOwnerId driverId -> do
        requireDriverInFleet fleetOwnerId driverId
        when (verb == Unlink) $ resolveRcId >>= requireRcInFleet fleetOwnerId
      ActorFleet fleetOwnerId ->
        when (verb == Unlink) $ resolveRcId >>= requireRcInFleet fleetOwnerId
      _ -> pure ()
    requireDriverInFleet fleetOwnerId driverId = do
      mbAssoc <- QFDA.findByDriverIdAndFleetOwnerId driverId fleetOwnerId.getId True
      when (isNothing mbAssoc) $ throwError DriverNotPartOfFleet
    requireRcInFleet fleetOwnerId mbRcId = do
      now <- getCurrentTime
      mbAssoc <- maybe (pure Nothing) (\rcId -> FRCA.findLinkedByRCIdAndFleetOwnerId fleetOwnerId rcId now) mbRcId
      when (isNothing mbAssoc) $ throwError VehicleNotPartOfFleet

guardFleetMembership :: OnboardingFlow m r => ActionVerb -> GuardTarget -> m ()
guardFleetMembership verb target = case (verb, target) of
  (ChangeFleetOwner, TargetDriver personId) -> do
    void $ fleetDriverInfo personId
    activeAssocs <- QFDA.findAllByDriverIdWithStatus personId
    when (null activeAssocs) $ throwError DriverHasNoActiveFleetAssociation
  (ChangeFleetOwner, _) -> throwError $ InvalidRequest "ChangeFleetOwner is only supported for driver targets"
  (LinkToFleet, TargetDriver personId) -> do
    driverInfo <- fleetDriverInfo personId
    mbActiveAssoc <- QFDA.findByDriverId personId True
    when (isJust mbActiveAssoc && driverInfo.enabled) $ throwError DriverAlreadyLinkedWithFleet
  _ -> pure ()
  where
    fleetDriverInfo personId = do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonDoesNotExist personId.getId)
      unless (driverInfo.onboardingAs == Just DI.FLEET_DRIVER) $ throwError DriverNotFleetDriver
      pure driverInfo

guardOnboardingAction :: OnboardingFlow m r => DTC.TransporterConfig -> Actor -> ActionVerb -> GuardTarget -> m ()
guardOnboardingAction transporterConfig actor verb target = do
  when (isUnified transporterConfig) $ do
    guardFleetMembership verb target
    guardFleetVehicleRelations verb target actor
    guardActorScope verb target actor
  guardNoLiveRide verb target
  guardAssociationAllowed verb target
  guardRcAssociationAllowed transporterConfig verb target
  when (isUnified transporterConfig) $ do
    guardActorAllowed verb target actor
    mbSnapshot <- loadSnapshot target
    whenJust mbSnapshot $ \snapshot ->
      case checkPrecondition verb snapshot of
        Right () -> pure ()
        Left violation -> reportViolation verb target violation

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
