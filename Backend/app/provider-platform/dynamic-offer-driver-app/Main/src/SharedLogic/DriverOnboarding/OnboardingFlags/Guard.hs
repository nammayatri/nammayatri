module SharedLogic.DriverOnboarding.OnboardingFlags.Guard
  ( ActionVerb (..),
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
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
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
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery

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
    | otherwise -> ok
  Disable
    | isJust driverInfo.disabledReasonFlag -> violate "DI-3" "driver is already disabled"
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
    | driverInfo.enabled -> violate "D-DELETE" "driver is enabled; disable before deleting"
    | otherwise -> ok
  Unblock
    | not driverInfo.blocked -> ok
    | otherwise -> ok
  Block -> ok
  Unlink -> ok
  Deactivate -> ok
  Add -> ok
  Approve -> ok
  Reject -> ok

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

checkFleet :: ActionVerb -> DFOI.FleetOwnerInformation -> Either GuardViolation ()
checkFleet verb fleetInfo = case verb of
  Enable
    | isNothing fleetInfo.disabledReasonFlag ->
      violate "FI-1" "fleet owner is not administratively disabled; enablement is derived from documents"
    | otherwise -> ok
  Disable
    | isJust fleetInfo.disabledReasonFlag -> violate "FI-1" "fleet owner is already disabled"
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
  | verb `notElem` [Unlink, Deactivate, Delete, Disable] = pure ()
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
  | verb `notElem` [Link, Add] = pure ()
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
  | verb `notElem` [Link, Add, Activate] = pure ()
  | transporterConfig.blockDriverOwnRCForFleetDrivers /= Just True = pure ()
  | otherwise = case target of
    TargetVehicleById rcId -> AC.guardRCNotActiveWithAnotherDriver rcId
    TargetVehicle registrationNo -> do
      mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
      whenJust mbRc $ \rc -> AC.guardRCNotActiveWithAnotherDriver rc.id
    _ -> pure ()

guardOnboardingAction :: OnboardingFlow m r => DTC.TransporterConfig -> ActionVerb -> GuardTarget -> m ()
guardOnboardingAction transporterConfig verb target = do
  guardNoLiveRide verb target
  guardAssociationAllowed verb target
  guardRcAssociationAllowed transporterConfig verb target
  when (isUnified transporterConfig) $ do
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

withOnboardingAction :: OnboardingFlow m r => DTC.TransporterConfig -> ActionVerb -> GuardTarget -> m a -> m a
withOnboardingAction transporterConfig verb target body =
  withOnboardingActionFanout transporterConfig verb target ((,mempty) <$> body)

withOnboardingActionFanout :: OnboardingFlow m r => DTC.TransporterConfig -> ActionVerb -> GuardTarget -> m (a, RecomputeSpec) -> m a
withOnboardingActionFanout transporterConfig verb target body =
  AC.withAssociation (guardOnboardingAction transporterConfig verb target) $ do
    (result, extraSpec) <- body
    when (isUnified transporterConfig) $
      runRecomputeSpec transporterConfig (defaultRecomputeSpec target <> extraSpec)
    pure result
