module SharedLogic.DriverOnboarding.OnboardingFlags.Checks where

import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverOperatorAssociation as DDOA
import qualified Domain.Types.DriverRCAssociation as DDRCA
import qualified Domain.Types.FleetDriverAssociation as DFDA
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleRegistrationCertificate as DVRC
import Kernel.Prelude
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverOperatorAssociationExtra as QDOA
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOperatorAssociation as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetRCAssociation as FRCA
import qualified Storage.Queries.Ride as QRide
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import Tools.Error

data ActionVerb
  = LinkVehicle
  | UnlinkVehicle
  | ActivateVehicle
  | DeactivateVehicle
  | LinkToOperator
  | UnlinkFromOperator
  | UnlinkFromFleet
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
  | ActivateToFleet
  | DeactivateFromFleet
  | View
  | ChangeFleetOwner
  | Expire
  | UnlinkDocument
  | OnboardingFlagMutation
  deriving (Show, Eq, Generic, Enum, Bounded)

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
  | TargetDriverVehicle (Id DP.Person) Text
  | TargetVehicle Text
  | TargetVehicleById (Id DVRC.VehicleRegistrationCertificate)
  | TargetFleetOwner (Id DP.Person)
  deriving (Show, Eq, Generic)

data GuardViolation = GuardViolation
  { gvInvariant :: Text,
    gvDetail :: Text
  }
  deriving (Show, Generic)

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

isUnified :: DTC.TransporterConfig -> Bool
isUnified transporterConfig = transporterConfig.unifiedOnboardingFlagsRecompute == Just True

ok :: Either GuardViolation ()
ok = Right ()

violate :: Text -> Text -> Either GuardViolation ()
violate invariant detail = Left $ GuardViolation invariant detail

ensure :: Bool -> Text -> Text -> Either GuardViolation ()
ensure condition invariant detail = if condition then ok else violate invariant detail

-------------------------------------------------------------------------------
-- The resolved context: everything a guard could ask about, loaded once.
-------------------------------------------------------------------------------

data ActorFleetCtx = ActorFleetCtx
  { afId :: Id DP.Person,
    afInfo :: Maybe DFOI.FleetOwnerInformation
  }

data ActorDriverCtx = ActorDriverCtx
  { adId :: Id DP.Person,
    adInfo :: Maybe DI.DriverInformation,
    adFleetAssociations :: [DFDA.FleetDriverAssociation]
  }

data DriverCtx = DriverCtx
  { dcId :: Id DP.Person,
    dcInfo :: DI.DriverInformation,
    dcFleetAssociations :: [DFDA.FleetDriverAssociation],
    dcOperatorAssociations :: [DDOA.DriverOperatorAssociation],
    dcMerchant :: Maybe DM.Merchant
  }

data VehicleCtx = VehicleCtx
  { vcRc :: DVRC.VehicleRegistrationCertificate,
    vcDriverAssociation :: Maybe DDRCA.DriverRCAssociation,
    vcInActorFleet :: Bool
  }

data FleetCtx = FleetCtx
  { fcId :: Id DP.Person,
    fcInfo :: DFOI.FleetOwnerInformation,
    fcRcAssociations :: Bool,
    fcDriverAssociations :: Bool,
    fcOperatorAssociations :: Bool
  }

data TargetEntity
  = EDriver DriverCtx
  | EDriverVehicle DriverCtx VehicleCtx
  | EVehicle VehicleCtx
  | EFleet FleetCtx
  | EUnresolved

data TargetCtx = TargetCtx
  { tcRaw :: GuardTarget,
    tcEntity :: TargetEntity,
    tcHasLiveRide :: Bool
  }

data ActorCtx = ActorCtx
  { acFleet :: Maybe ActorFleetCtx,
    acDriver :: Maybe ActorDriverCtx
  }

data GuardCtx = GuardCtx
  { gcConfig :: DTC.TransporterConfig,
    gcVerb :: ActionVerb,
    gcActor :: ActorCtx,
    gcTarget :: TargetCtx
  }

-------------------------------------------------------------------------------
-- Resolution
-------------------------------------------------------------------------------

resolveCtx :: OnboardingFlow m r => DTC.TransporterConfig -> Actor -> ActionVerb -> GuardTarget -> m GuardCtx
resolveCtx transporterConfig actor verb target = do
  actorCtx <- resolveActor verb actor
  targetCtx <- resolveTarget verb (afId <$> actorCtx.acFleet) target
  pure GuardCtx {gcConfig = transporterConfig, gcVerb = verb, gcActor = actorCtx, gcTarget = targetCtx}

resolveActor :: OnboardingFlow m r => ActionVerb -> Actor -> m ActorCtx
resolveActor verb = \case
  None -> pure $ ActorCtx Nothing Nothing
  ActorFleet fleetOwnerId -> do
    fleetCtx <- resolveActorFleet fleetOwnerId
    pure $ ActorCtx (Just fleetCtx) Nothing
  ActorDriver driverId -> do
    driverCtx <- resolveActorDriver verb driverId
    pure $ ActorCtx Nothing (Just driverCtx)
  ActorFleetAndDriver fleetOwnerId driverId -> do
    fleetCtx <- resolveActorFleet fleetOwnerId
    driverCtx <- resolveActorDriver verb driverId
    pure $ ActorCtx (Just fleetCtx) (Just driverCtx)

resolveActorFleet :: OnboardingFlow m r => Id DP.Person -> m ActorFleetCtx
resolveActorFleet fleetOwnerId = ActorFleetCtx fleetOwnerId <$> QFOI.findByPrimaryKey fleetOwnerId

resolveActorDriver :: OnboardingFlow m r => ActionVerb -> Id DP.Person -> m ActorDriverCtx
resolveActorDriver verb driverId = do
  info <- DIQuery.findById (cast driverId)
  fleetAssociations <-
    if verb `elem` [LinkVehicle, UnlinkVehicle]
      then QFDA.findAllByDriverIdWithStatus driverId
      else pure []
  pure $ ActorDriverCtx driverId info fleetAssociations

resolveTarget :: OnboardingFlow m r => ActionVerb -> Maybe (Id DP.Person) -> GuardTarget -> m TargetCtx
resolveTarget verb mbActorFleetId target = do
  entity <- case target of
    TargetDriver personId -> maybe EUnresolved EDriver <$> resolveDriver personId
    TargetDriverVehicle personId registrationNo -> do
      mbDriver <- resolveDriver personId
      mbVehicle <- resolveVehicleByRegistrationNo mbActorFleetId (Just personId) registrationNo
      pure $ case (mbDriver, mbVehicle) of
        (Just driverCtx, Just vehicleCtx) -> EDriverVehicle driverCtx vehicleCtx
        (Just driverCtx, Nothing) -> EDriver driverCtx
        _ -> EUnresolved
    TargetVehicle registrationNo -> maybe EUnresolved EVehicle <$> resolveVehicleByRegistrationNo mbActorFleetId Nothing registrationNo
    TargetVehicleById rcId -> do
      mbRc <- RCQuery.findById rcId
      maybe (pure EUnresolved) (fmap EVehicle . resolveVehicle mbActorFleetId Nothing) mbRc
    TargetFleetOwner personId -> maybe EUnresolved EFleet <$> resolveFleetOwner verb personId
  hasLiveRide <- if verb `elem` liveRideVerbs then resolveLiveRide entity target else pure False
  pure TargetCtx {tcRaw = target, tcEntity = entity, tcHasLiveRide = hasLiveRide}

resolveDriver :: OnboardingFlow m r => Id DP.Person -> m (Maybe DriverCtx)
resolveDriver personId = do
  mbInfo <- DIQuery.findById (cast personId)
  forM mbInfo $ \info -> do
    fleetAssociations <- QFDA.findAllByDriverIdWithStatus personId
    operatorAssociations <- QDOA.findAllByDriverId personId True
    merchant <- maybe (pure Nothing) CQM.findById info.merchantId
    pure
      DriverCtx
        { dcId = personId,
          dcInfo = info,
          dcFleetAssociations = fleetAssociations,
          dcOperatorAssociations = operatorAssociations,
          dcMerchant = merchant
        }

resolveVehicleByRegistrationNo :: OnboardingFlow m r => Maybe (Id DP.Person) -> Maybe (Id DP.Person) -> Text -> m (Maybe VehicleCtx)
resolveVehicleByRegistrationNo mbActorFleetId mbDriverId registrationNo = do
  mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
  forM mbRc $ resolveVehicle mbActorFleetId mbDriverId

resolveVehicle :: OnboardingFlow m r => Maybe (Id DP.Person) -> Maybe (Id DP.Person) -> DVRC.VehicleRegistrationCertificate -> m VehicleCtx
resolveVehicle mbActorFleetId mbDriverId rc = do
  now <- getCurrentTime
  driverAssociation <- maybe (pure Nothing) (\driverId -> DRAQuery.findLinkedByRCIdAndDriverId (cast driverId) rc.id now) mbDriverId
  inActorFleet <- maybe (pure False) (\fleetOwnerId -> isJust <$> FRCA.findLinkedByRCIdAndFleetOwnerId fleetOwnerId rc.id now) mbActorFleetId
  pure VehicleCtx {vcRc = rc, vcDriverAssociation = driverAssociation, vcInActorFleet = inActorFleet}

resolveFleetOwner :: OnboardingFlow m r => ActionVerb -> Id DP.Person -> m (Maybe FleetCtx)
resolveFleetOwner verb personId = do
  mbInfo <- QFOI.findByPrimaryKey personId
  forM mbInfo $ \info ->
    if verb == Delete
      then do
        rcAssociations <- isJust <$> FRCA.findActiveAssociationByFleetOwnerId personId
        driverAssociations <- isJust <$> QFDA.findActiveDriverByFleetOwnerId personId.getId
        operatorAssociations <- isJust <$> QFOA.findActiveByFleetOwnerId personId
        pure FleetCtx {fcId = personId, fcInfo = info, fcRcAssociations = rcAssociations, fcDriverAssociations = driverAssociations, fcOperatorAssociations = operatorAssociations}
      else pure FleetCtx {fcId = personId, fcInfo = info, fcRcAssociations = False, fcDriverAssociations = False, fcOperatorAssociations = False}

-- | Only the teardown verbs care, so the ride lookups stay off every other path.
liveRideVerbs :: [ActionVerb]
liveRideVerbs =
  [ UnlinkVehicle,
    DeactivateVehicle,
    DeactivateFromFleet,
    UnlinkFromOperator,
    UnlinkFromFleet,
    Delete,
    Disable,
    Expire,
    SetOnboardingAs,
    UnlinkDocument,
    ChangeFleetOwner,
    Block,
    OnboardingFlagMutation
  ]

-- | An RC-scoped action resolves live rides through the RC's active driver and a fleet-scoped one
--   across every active driver in the fleet.
resolveLiveRide :: OnboardingFlow m r => TargetEntity -> GuardTarget -> m Bool
resolveLiveRide entity target = case entity of
  EDriver driverCtx -> liveRideOfDriver driverCtx.dcId
  EDriverVehicle driverCtx _ -> liveRideOfDriver driverCtx.dcId
  EVehicle vehicleCtx -> liveRideOfVehicle vehicleCtx.vcRc.id
  EFleet fleetCtx -> liveRideInFleet fleetCtx.fcId
  EUnresolved -> case target of
    TargetDriver personId -> liveRideOfDriver personId
    TargetDriverVehicle personId _ -> liveRideOfDriver personId
    TargetFleetOwner personId -> liveRideInFleet personId
    _ -> pure False

liveRideOfDriver :: OnboardingFlow m r => Id DP.Person -> m Bool
liveRideOfDriver personId = isJust <$> QRide.getUpcomingOrActiveByDriverId personId

liveRideOfVehicle :: OnboardingFlow m r => Id DVRC.VehicleRegistrationCertificate -> m Bool
liveRideOfVehicle rcId = do
  mbAssoc <- DRAQuery.findActiveAssociationByRC rcId True
  maybe (pure False) (\assoc -> isJust <$> QRide.findFirstUpcomingOrActiveByDriverIds [assoc.driverId]) mbAssoc

liveRideInFleet :: OnboardingFlow m r => Id DP.Person -> m Bool
liveRideInFleet fleetOwnerId = do
  driverIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId.getId
  isJust <$> QRide.findFirstUpcomingOrActiveByDriverIds driverIds

-------------------------------------------------------------------------------
-- The three columns of a guard line
-------------------------------------------------------------------------------

type Guard m = ReaderT GuardCtx m ()

class VerbMatch v where
  matchVerb :: v -> ActionVerb -> Bool

instance VerbMatch ActionVerb where
  matchVerb = (==)

instance VerbMatch [ActionVerb] where
  matchVerb = flip elem

data AnyVerb = AnyVerb

instance VerbMatch AnyVerb where
  matchVerb _ _ = True

data ActorSel a where
  AnyActor :: ActorSel ()
  FleetActor :: ActorSel ActorFleetCtx
  DriverActor :: ActorSel ActorDriverCtx
  FleetAndDriverActor :: ActorSel (ActorFleetCtx, ActorDriverCtx)

data TargetSel a where
  AnyTarget :: TargetSel TargetCtx
  DriverTarget :: TargetSel DriverCtx
  VehicleTarget :: TargetSel VehicleCtx
  DriverVehicle :: TargetSel (DriverCtx, VehicleCtx)
  RcTarget :: TargetSel VehicleCtx
  FleetTarget :: TargetSel FleetCtx

matchActor :: ActorSel a -> GuardCtx -> Maybe a
matchActor selector ctx = case selector of
  AnyActor -> Just ()
  FleetActor -> ctx.gcActor.acFleet
  DriverActor -> ctx.gcActor.acDriver
  FleetAndDriverActor -> (,) <$> ctx.gcActor.acFleet <*> ctx.gcActor.acDriver

matchTarget :: TargetSel a -> GuardCtx -> Maybe a
matchTarget selector ctx = case (selector, ctx.gcTarget.tcEntity) of
  (AnyTarget, _) -> Just ctx.gcTarget
  (DriverTarget, EDriver driverCtx) -> Just driverCtx
  (DriverTarget, EDriverVehicle driverCtx _) -> Just driverCtx
  (VehicleTarget, EVehicle vehicleCtx) -> Just vehicleCtx
  (DriverVehicle, EDriverVehicle driverCtx vehicleCtx) -> Just (driverCtx, vehicleCtx)
  (RcTarget, EVehicle vehicleCtx) -> Just vehicleCtx
  (RcTarget, EDriverVehicle _ vehicleCtx) -> Just vehicleCtx
  (FleetTarget, EFleet fleetCtx) -> Just fleetCtx
  _ -> Nothing

-- | A guard line: verb, actor, target, and a pure check over what they resolved to.
check :: (VerbMatch v, OnboardingFlow m r) => v -> ActorSel a -> TargetSel b -> (a -> b -> Either GuardViolation ()) -> Guard m
check verbMatch actorSel targetSel pureCheck = checkV verbMatch actorSel targetSel (const pureCheck)

-- | For the few checks that also name the verb.
checkV :: (VerbMatch v, OnboardingFlow m r) => v -> ActorSel a -> TargetSel b -> (ActionVerb -> a -> b -> Either GuardViolation ()) -> Guard m
checkV verbMatch actorSel targetSel pureCheck =
  onMatch verbMatch actorSel targetSel $ \ctx actorPart targetPart ->
    either (report ctx) pure (pureCheck ctx.gcVerb actorPart targetPart)

-- | For the one guard that also mutates.
checkM :: (VerbMatch v, OnboardingFlow m r) => v -> ActorSel a -> TargetSel b -> (a -> b -> m ()) -> Guard m
checkM verbMatch actorSel targetSel run = onMatch verbMatch actorSel targetSel (const run)

onMatch :: (VerbMatch v, OnboardingFlow m r) => v -> ActorSel a -> TargetSel b -> (GuardCtx -> a -> b -> m ()) -> Guard m
onMatch verbMatch actorSel targetSel run =
  ask >>= \ctx ->
    when (matchVerb verbMatch ctx.gcVerb) $
      whenJust ((,) <$> matchActor actorSel ctx <*> matchTarget targetSel ctx) $
        \(actorPart, targetPart) -> lift $ run ctx actorPart targetPart

report :: OnboardingFlow m r => GuardCtx -> GuardViolation -> m ()
report ctx violation = throwError $ OnboardingActionNotAllowed (show ctx.gcVerb, violation.gvDetail, violation.gvInvariant)

unified :: OnboardingFlow m r => Guard m -> Guard m
unified guardFlow = ask >>= \ctx -> when (isUnified ctx.gcConfig) guardFlow

whenConfig :: OnboardingFlow m r => (DTC.TransporterConfig -> Bool) -> Guard m -> Guard m
whenConfig predicate guardFlow = ask >>= \ctx -> when (predicate ctx.gcConfig) guardFlow

blocksDriverOwnRc :: DTC.TransporterConfig -> Bool
blocksDriverOwnRc transporterConfig = transporterConfig.blockDriverOwnRCForFleetDrivers == Just True

-------------------------------------------------------------------------------
-- The flow: verb, actor, target, guard -- every matching line runs, in order.
-------------------------------------------------------------------------------

onboardingFlow :: OnboardingFlow m r => Guard m
onboardingFlow = do
  unified $ check ChangeFleetOwner AnyActor AnyTarget movableTarget -- only a driver or a vehicle moves fleet
  unified $ check ChangeFleetOwner AnyActor DriverTarget movableFleetDriver -- fleet driver with a live association
  unified $ check ChangeFleetOwner AnyActor VehicleTarget movableFleetVehicle -- vehicle currently held by a fleet
  unified $ check [LinkToFleet, LinkToOperator] AnyActor DriverTarget notLiveInAnotherFleet -- a live driver moves, not links
  unified $ check [LinkToFleet, LinkToOperator] FleetActor DriverTarget notActiveWithActorFleet -- no active link to the same fleet
  unified $ check ActivateToFleet FleetActor DriverTarget actorFleetAssociationExists -- needs a link with this fleet
  unified $ check DeactivateFromFleet FleetActor DriverTarget actorFleetAssociationExists -- needs a link with this fleet
  unified $ check ActivateVehicle AnyActor DriverVehicle rcLinkedAndInactive -- RC linked and not already driven
  unified $ check DeactivateVehicle AnyActor DriverVehicle rcLinkedAndActive -- RC linked and currently driven
  unified $ check LinkVehicle FleetAndDriverActor VehicleTarget actingDriverActiveInActorFleet -- acting driver belongs to the fleet
  unified $ check LinkVehicle FleetActor VehicleTarget vehicleNotInAnotherFleet
  unified $ check UnlinkVehicle FleetActor VehicleTarget vehicleInActorFleet -- vehicle belongs to the fleet
  unified $ check View FleetActor AnyTarget actorScopeOverTarget -- a fleet reads only what it holds
  forM_ liveRideVerbs $ \verb -> check verb AnyActor AnyTarget noLiveRide -- no change under a live ride
  check [LinkToFleet, LinkToOperator] AnyActor DriverTarget ensureNoActiveFleetAssociation -- no live fleet or operator association
  check Delete AnyActor DriverTarget driverDeletable -- driver strands no association
  check Delete AnyActor FleetTarget fleetDeletable -- fleet strands no association
  whenConfig blocksDriverOwnRc $ do
    checkM LinkVehicle AnyActor RcTarget rcNotActiveWithAnotherDriver -- RC not driven by someone else
    checkM ActivateVehicle AnyActor RcTarget rcNotActiveWithAnotherDriver -- RC not driven by someone else
  unified $ check AnyVerb FleetActor AnyTarget actingFleetOwnerFit -- acting fleet owner is live
  unified $ check LinkVehicle DriverActor AnyTarget actingDriverFit -- acting driver is live
  unified $ check ActivateVehicle DriverActor AnyTarget actingDriverFit -- acting driver is live
  unified $ check Enable AnyActor DriverTarget driverEnableable -- only a driver who is down comes up
  unified $ check Disable AnyActor DriverTarget driverDisableable -- only a live driver goes down
  unified $ check Block AnyActor DriverTarget driverBlockable -- only a live, unblocked driver is barred
  unified $ check Unblock AnyActor DriverTarget driverUnblockable -- nothing to lift without a block
  unified $ check ActivateVehicle AnyActor DriverTarget driverLiveForRcPick -- must be live to pick up an RC
  unified $ check ActivateToFleet AnyActor DriverTarget driverNotBlocked -- a blocked driver joins no fleet
  unified $ check SetOnboardingAs AnyActor DriverTarget driverOnboardingSettable -- settled before the driver goes live
  unified $ check [LinkToFleet, LinkToOperator] AnyActor DriverTarget driverLinkableToFleet -- an active driver moves instead
  unified $ check LinkToFleet AnyActor DriverTarget driverNotSelfEmployed
  unified $ check LinkVehicle AnyActor VehicleTarget rcClearedForRoad -- RC valid, verified and approved
  unified $ check Enable AnyActor FleetTarget fleetEnableable -- only a fleet owner who is down comes up
  unified $ check Disable AnyActor FleetTarget fleetDisableable -- only a live fleet owner goes down
  unified $ check Block AnyActor FleetTarget fleetBlockable -- only a live, unblocked fleet owner is barred
  unified $ check Unblock AnyActor FleetTarget fleetUnblockable -- nothing to lift without a block
  where
    movableTarget :: () -> TargetCtx -> Either GuardViolation ()
    movableTarget _ targetCtx = case targetCtx.tcRaw of
      TargetDriver _ -> ok
      TargetVehicle _ -> ok
      TargetVehicleById _ -> ok
      _ -> violate "SCOPE-TARGET" "changeFleetOwner is only supported for driver or vehicle targets"
    movableFleetDriver :: () -> DriverCtx -> Either GuardViolation ()
    movableFleetDriver _ driverCtx = do
      ensure (driverCtx.dcInfo.onboardingAs == Just DI.FLEET_DRIVER) "FD-ROLE" "driver is not a fleet driver"
      ensure (not (null driverCtx.dcFleetAssociations)) "FD-NONE" "driver has no active fleet association"
    movableFleetVehicle :: () -> VehicleCtx -> Either GuardViolation ()
    movableFleetVehicle _ vehicleCtx =
      ensure (isJust vehicleCtx.vcRc.fleetOwnerId) "FR-NONE" "vehicle is not held by any fleet"
    notLiveInAnotherFleet :: () -> DriverCtx -> Either GuardViolation ()
    notLiveInAnotherFleet _ driverCtx =
      unless (fleetDriverWithoutActiveAssociation driverCtx) $
        ensure (not (hasActiveFleetAssociation driverCtx && driverCtx.dcInfo.enabled)) "FD-LIVE" "driver is enabled and already linked with an active fleet association"
    notActiveWithActorFleet :: ActorFleetCtx -> DriverCtx -> Either GuardViolation ()
    notActiveWithActorFleet actorFleet driverCtx =
      ensure (not (isActivelyAssociatedWith actorFleet.afId driverCtx)) "FD-DUPLICATE" "driver already has an active association with this fleet"
    actorFleetAssociationExists :: ActorFleetCtx -> DriverCtx -> Either GuardViolation ()
    actorFleetAssociationExists actorFleet driverCtx =
      ensure (isAssociatedWith actorFleet.afId driverCtx) "FD-MISSING" "driver holds no association with this fleet"
    rcLinkedAndInactive :: () -> (DriverCtx, VehicleCtx) -> Either GuardViolation ()
    rcLinkedAndInactive _ (_, vehicleCtx) = do
      assoc <- requireRcAssociation vehicleCtx
      ensure (not assoc.isRcActive) "RC-ACTIVE" "vehicle is already the driver's active vehicle"
    rcLinkedAndActive :: () -> (DriverCtx, VehicleCtx) -> Either GuardViolation ()
    rcLinkedAndActive _ (_, vehicleCtx) = do
      assoc <- requireRcAssociation vehicleCtx
      ensure assoc.isRcActive "RC-INACTIVE" "vehicle is not the driver's active vehicle"

    requireRcAssociation :: VehicleCtx -> Either GuardViolation DDRCA.DriverRCAssociation
    requireRcAssociation vehicleCtx = maybe (Left $ GuardViolation "RC-UNLINKED" "vehicle is not linked with the driver") Right vehicleCtx.vcDriverAssociation
    actingDriverActiveInActorFleet :: (ActorFleetCtx, ActorDriverCtx) -> VehicleCtx -> Either GuardViolation ()
    actingDriverActiveInActorFleet (actorFleet, actorDriver) _ =
      ensure (any (\assoc -> assoc.fleetOwnerId == actorFleet.afId.getId && assoc.isActive) actorDriver.adFleetAssociations) "FD-MISSING" "driver is not part of this fleet"
    vehicleNotInAnotherFleet :: ActorFleetCtx -> VehicleCtx -> Either GuardViolation ()
    vehicleNotInAnotherFleet actorFleet vehicleCtx =
      ensure (maybe True (== actorFleet.afId.getId) vehicleCtx.vcRc.fleetOwnerId) "FR-OTHER-FLEET" "vehicle is already held by another fleet"
    vehicleInActorFleet :: ActorFleetCtx -> VehicleCtx -> Either GuardViolation ()
    vehicleInActorFleet _ vehicleCtx =
      ensure vehicleCtx.vcInActorFleet "FR-MISSING" "vehicle is not part of this fleet"
    actorScopeOverTarget :: ActorFleetCtx -> TargetCtx -> Either GuardViolation ()
    actorScopeOverTarget actorFleet targetCtx = case targetCtx.tcEntity of
      EDriver driverCtx -> driverScope driverCtx
      EDriverVehicle driverCtx _ -> driverScope driverCtx
      EVehicle vehicleCtx -> ensure (vehicleCtx.vcRc.fleetOwnerId == Just actorFleet.afId.getId) "SCOPE-VEHICLE" "vehicle is not part of this fleet"
      EFleet fleetCtx -> ensure (fleetCtx.fcId == actorFleet.afId) "SCOPE-FLEET" "fleet owner is not the acting fleet owner"
      EUnresolved -> ok
      where
        driverScope driverCtx = ensure (isAssociatedWith actorFleet.afId driverCtx) "SCOPE-DRIVER" "driver is not part of this fleet"
    noLiveRide :: () -> TargetCtx -> Either GuardViolation ()
    noLiveRide _ targetCtx = ensure (not targetCtx.tcHasLiveRide) "LIVE-RIDE" "a live ride is in progress, cannot change association"
    ensureNoActiveFleetAssociation :: () -> DriverCtx -> Either GuardViolation ()
    ensureNoActiveFleetAssociation _ driverCtx =
      unless (maybe False (\merchant -> merchant.overwriteAssociation == Just True) driverCtx.dcMerchant) $ do
        ensure (not (hasActiveFleetAssociation driverCtx)) "A-FLEET" "driver already associated with a fleet"
        ensure (null driverCtx.dcOperatorAssociations) "A-OPERATOR" "driver is already associated with an operator"
    driverDeletable :: () -> DriverCtx -> Either GuardViolation ()
    driverDeletable _ driverCtx = do
      ensure (not (hasActiveFleetAssociation driverCtx)) "DEL-FLEET" "cannot delete driver with active fleet associations"
      ensure (null driverCtx.dcOperatorAssociations) "DEL-OPERATOR" "cannot delete driver with active operator associations"

    fleetDeletable :: () -> FleetCtx -> Either GuardViolation ()
    fleetDeletable _ fleetCtx = do
      ensure (not fleetCtx.fcRcAssociations) "DEL-RC" "cannot delete fleet owner with active RC associations"
      ensure (not fleetCtx.fcDriverAssociations) "DEL-DRIVER" "cannot delete fleet owner with active driver associations"
      ensure (not fleetCtx.fcOperatorAssociations) "DEL-OPERATOR" "cannot delete fleet owner with active operator associations"
    rcNotActiveWithAnotherDriver :: OnboardingFlow m r => () -> VehicleCtx -> m ()
    rcNotActiveWithAnotherDriver _ vehicleCtx = do
      let rcId = vehicleCtx.vcRc.id
      hasLiveRide <- liveRideOfVehicle rcId
      when hasLiveRide $ throwError (InvalidRequest "Vehicle has a live ride, cannot change association")
      mbActiveDriverAssoc <- DRAQuery.findActiveAssociationByRC rcId True
      whenJust mbActiveDriverAssoc $ \_ -> throwError RCActiveOnOtherAccount
      linkedDriverAssocs <- DRAQuery.findAllActiveAssociationByRCId rcId
      forM_ linkedDriverAssocs $ \assoc -> DRAQuery.endAssociationForRC assoc.driverId rcId
    actingFleetOwnerFit :: ActorFleetCtx -> TargetCtx -> Either GuardViolation ()
    actingFleetOwnerFit actorFleet targetCtx = case (targetCtx.tcEntity, actorFleet.afInfo) of
      (EFleet _, _) -> ok
      (_, Nothing) -> ok
      (_, Just fleetInfo) -> do
        ensure (not fleetInfo.blocked) "ACTOR-2" "acting fleet owner is blocked"
        ensure (isNothing fleetInfo.disabledReasonFlag) "ACTOR-3" "acting fleet owner is disabled"
        ensure fleetInfo.enabled "ACTOR-1" "acting fleet owner is not enabled"
    actingDriverFit :: ActorDriverCtx -> TargetCtx -> Either GuardViolation ()
    actingDriverFit actorDriver targetCtx = case (targetCtx.tcEntity, actorDriver.adInfo) of
      (EFleet _, _) -> ok
      (_, Nothing) -> ok
      (_, Just driverInfo) -> do
        ensure (not driverInfo.blocked) "ACTOR-2" "acting driver is blocked"
        ensure (isNothing driverInfo.disabledReasonFlag) "ACTOR-3" "acting driver is disabled"
        ensure driverInfo.enabled "ACTOR-1" "acting driver is not enabled"
    driverEnableable :: () -> DriverCtx -> Either GuardViolation ()
    driverEnableable _ driverCtx = do
      ensure (not driverCtx.dcInfo.blocked) "D-BLOCKED" "driver is blocked, unblock before enabling"
      ensure (isJust driverCtx.dcInfo.disabledReasonFlag || not driverCtx.dcInfo.enabled) "D-UNSUPPORTED" "driver is already in enabled state"
    driverDisableable :: () -> DriverCtx -> Either GuardViolation ()
    driverDisableable _ driverCtx = do
      ensure driverCtx.dcInfo.enabled "DI-2" "driver is not enabled; enablement is derived from documents or admin enablement"
      ensure (isNothing driverCtx.dcInfo.disabledReasonFlag) "DI-3" "driver is already disabled"
    driverBlockable :: () -> DriverCtx -> Either GuardViolation ()
    driverBlockable _ driverCtx = do
      ensure (not driverCtx.dcInfo.blocked) "D-BLOCKED" "driver is already blocked"
      ensure driverCtx.dcInfo.enabled "DI-2" "driver is not enabled; enablement is derived from documents or admin enablement"
      ensure (isNothing driverCtx.dcInfo.disabledReasonFlag) "DI-1" "driver is disabled, enable the driver before blocking"
    driverUnblockable :: () -> DriverCtx -> Either GuardViolation ()
    driverUnblockable _ driverCtx = ensure driverCtx.dcInfo.blocked "D-BLOCKED" "driver is not blocked"
    driverLiveForRcPick :: () -> DriverCtx -> Either GuardViolation ()
    driverLiveForRcPick _ driverCtx = do
      ensure driverCtx.dcInfo.enabled "DI-1" "driver is not enabled"
      ensure (isNothing driverCtx.dcInfo.disabledReasonFlag) "DI-3" "driver is disabled"
      ensure (not driverCtx.dcInfo.blocked) "D-BLOCKED" "driver is blocked"
    driverNotBlocked :: () -> DriverCtx -> Either GuardViolation ()
    driverNotBlocked _ driverCtx = ensure (not driverCtx.dcInfo.blocked) "D-BLOCKED" "driver is blocked"
    driverOnboardingSettable :: () -> DriverCtx -> Either GuardViolation ()
    driverOnboardingSettable _ driverCtx = do
      ensure (not driverCtx.dcInfo.blocked) "DI-9" "driver is blocked"
      ensure (isNothing driverCtx.dcInfo.disabledReasonFlag) "DI-10" "driver is disabled"
    driverNotSelfEmployed :: () -> DriverCtx -> Either GuardViolation ()
    driverNotSelfEmployed _ driverCtx =
      ensure (driverCtx.dcInfo.onboardingAs /= Just DI.INDIVIDUAL) "FD-INDIVIDUAL" "driver is onboarded as self-employed; change the onboarding type before linking to a fleet"
    driverLinkableToFleet :: () -> DriverCtx -> Either GuardViolation ()
    driverLinkableToFleet _ driverCtx =
      unless (fleetDriverWithoutActiveAssociation driverCtx) $
        ensure (not driverCtx.dcInfo.enabled) "DI-9" "driver is already enabled; use changeFleetOwner to move an active driver between fleets"
    rcClearedForRoad :: () -> VehicleCtx -> Either GuardViolation ()
    rcClearedForRoad _ vehicleCtx = do
      ensure (vehicleCtx.vcRc.verificationStatus == Documents.VALID) "RI-2" "RC verification status is not VALID"
      ensure (vehicleCtx.vcRc.verified == Just True) "RI-2" "RC is not verified"
      ensure (vehicleCtx.vcRc.approved == Just True) "RI-1" "RC is not approved"
    fleetEnableable :: () -> FleetCtx -> Either GuardViolation ()
    fleetEnableable _ fleetCtx = do
      ensure (not fleetCtx.fcInfo.blocked) "F-BLOCKED" "fleet owner is blocked, unblock before enabling"
      ensure (isJust fleetCtx.fcInfo.disabledReasonFlag || not fleetCtx.fcInfo.enabled) "F-UNSUPPORTED" "fleet owner is already in enabled state"

    fleetDisableable :: () -> FleetCtx -> Either GuardViolation ()
    fleetDisableable _ fleetCtx = do
      ensure fleetCtx.fcInfo.enabled "FI-2" "fleet owner is not enabled; enablement is derived from documents or admin enablement"
      ensure (isNothing fleetCtx.fcInfo.disabledReasonFlag) "FI-1" "fleet owner is already disabled"

    fleetBlockable :: () -> FleetCtx -> Either GuardViolation ()
    fleetBlockable _ fleetCtx = do
      ensure (not fleetCtx.fcInfo.blocked) "F-BLOCKED" "fleet owner is already blocked"
      ensure fleetCtx.fcInfo.enabled "FI-2" "fleet owner is not enabled; enablement is derived from documents or admin enablement"
      ensure (isNothing fleetCtx.fcInfo.disabledReasonFlag) "FI-1" "fleet owner is disabled, enable the fleet owner before blocking"

    fleetUnblockable :: () -> FleetCtx -> Either GuardViolation ()
    fleetUnblockable _ fleetCtx = ensure fleetCtx.fcInfo.blocked "F-BLOCKED" "fleet owner is not blocked"

    hasActiveFleetAssociation :: DriverCtx -> Bool
    hasActiveFleetAssociation driverCtx = any (.isActive) driverCtx.dcFleetAssociations

    isAssociatedWith :: Id DP.Person -> DriverCtx -> Bool
    isAssociatedWith fleetOwnerId driverCtx = any (\assoc -> assoc.fleetOwnerId == fleetOwnerId.getId) driverCtx.dcFleetAssociations

    isActivelyAssociatedWith :: Id DP.Person -> DriverCtx -> Bool
    isActivelyAssociatedWith fleetOwnerId driverCtx = any (\assoc -> assoc.fleetOwnerId == fleetOwnerId.getId && assoc.isActive) driverCtx.dcFleetAssociations

    fleetDriverWithoutActiveAssociation :: DriverCtx -> Bool
    fleetDriverWithoutActiveAssociation driverCtx =
      driverCtx.dcInfo.onboardingAs == Just DI.FLEET_DRIVER && not (hasActiveFleetAssociation driverCtx)

guardOnboardingAction :: OnboardingFlow m r => DTC.TransporterConfig -> Actor -> ActionVerb -> GuardTarget -> m ()
guardOnboardingAction transporterConfig actor verb target =
  resolveCtx transporterConfig actor verb target >>= runReaderT onboardingFlow
