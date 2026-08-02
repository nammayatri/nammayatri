-- | The onboarding flag recompute: one entry point that groups a document set by the entity whose
--   flags it drives (fleet owner / driver from the config source, plus each vehicle), and the
--   per-entity arms that derive and persist verified / approved / enabled.
module SharedLogic.DriverOnboarding.OnboardingFlags.Flow
  ( -- * The one entry point for onboarding flag writes
    recomputeOnboardingFlags,
    OnboardingFlagsInput (..),
    OnboardingFlagsResult (..),
    OnboardingCounterEntity (..),
    OnboardingBuckets (..),
    OnboardingCounts (..),
    readOnboardingCounts,
    BlockChange (..),
    BlockPayload (..),
    SimplePayload (..),
    markBlockFlags,
    DisabledChange (..),
    markDisabledFlags,
    PersonFlagsCtx (..),
    VehicleDocsEntry (..),
  )
where

import Control.Applicative ((<|>))
import qualified Domain.Types.Common as Common
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.Common
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverInformationExtra as DIQueryExtra
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCEQuery
import Tools.Error (BlockReasonFlag (..))

data VehicleDocsEntry = VehicleDocsEntry
  { vdeRegistrationNo :: Text,
    vdeItem :: VehicleDocumentItem,
    vdeConfigs :: [DVC.DocumentVerificationConfig],
    -- | statusHandler passes the caller's value; the standalone vehicle recompute passes Nothing.
    --   The two differ today and both are preserved.
    vdeMakeSelfieAadhaarPanMandatory :: Maybe Bool
  }

-- | Everything the person-side recompute needs besides the documents themselves.
data PersonFlagsCtx = PersonFlagsCtx
  { pfcPerson :: DP.Person,
    pfcMerchantOpCityId :: Id DMOC.MerchantOperatingCity,
    pfcMerchantId :: Id DM.Merchant,
    pfcTransporterConfig :: DTC.TransporterConfig,
    pfcConfigs :: DocVerificationConfigs,
    pfcDocs :: [DocumentStatusItem],
    pfcVehicleCategory :: DVC.VehicleCategory,
    pfcMakeSelfieAadhaarPanMandatory :: Maybe Bool,
    pfcDriverName :: Maybe Text,
    pfcOnboardingVehicleCategory :: Maybe DVC.VehicleCategory,
    pfcIsFleetDriver :: Maybe Bool
  }

data OnboardingFlagsInput = OnboardingFlagsInput
  { ofiPerson :: Maybe PersonFlagsCtx,
    ofiVehicles :: [VehicleDocsEntry]
  }

data OnboardingFlagsResult = OnboardingFlagsResult
  { ofrPersonEnabled :: Maybe Bool,
    ofrVehiclesTouched :: Int
  }

-- | The single entry point for onboarding flag writes. Documents are grouped by the entity whose
--   flags they drive: the person group becomes fleet-owner or driver flags (chosen by the config
--   source -- Left is FleetOwnerDocumentVerificationConfig), and each vehicle entry drives its own
--   RC. Whichever groups are supplied get recomputed.
--
--   The person group is dispatched on the config source rather than on the document list being
--   non-empty, which preserves today's behaviour: an empty document list is vacuously valid, so a
--   driver with no documents is still recomputed rather than silently skipped.
recomputeOnboardingFlags ::
  OnboardingFlow m r =>
  OnboardingFlagsInput ->
  Bool ->
  m OnboardingFlagsResult
recomputeOnboardingFlags input useUnifiedOnboardingFlagsRecompute = do
  personEnabled <- case input.ofiPerson of
    Nothing -> pure Nothing
    Just pfc -> do
      let (personSideDocs, _vehicleSideDocs) = partitionDocsBySide pfc.pfcConfigs pfc.pfcDocs
      case pfc.pfcConfigs of
        Left _ ->
          Just
            <$> recomputeFleetFlagsArm
              pfc.pfcPerson
              pfc.pfcConfigs
              personSideDocs
              pfc.pfcVehicleCategory
              pfc.pfcMakeSelfieAadhaarPanMandatory
              useUnifiedOnboardingFlagsRecompute
        Right _ ->
          Just
            <$> recomputeDriverFlagsArm
              pfc.pfcMerchantOpCityId
              pfc.pfcMerchantId
              pfc.pfcPerson
              pfc.pfcConfigs
              personSideDocs
              pfc.pfcVehicleCategory
              pfc.pfcMakeSelfieAadhaarPanMandatory
              pfc.pfcDriverName
              pfc.pfcOnboardingVehicleCategory
              pfc.pfcTransporterConfig
              pfc.pfcIsFleetDriver
              useUnifiedOnboardingFlagsRecompute
  forM_ input.ofiVehicles $ \entry ->
    recomputeVehicleFlagsArm entry.vdeRegistrationNo entry.vdeItem entry.vdeConfigs entry.vdeMakeSelfieAadhaarPanMandatory useUnifiedOnboardingFlagsRecompute
  pure OnboardingFlagsResult {ofrPersonEnabled = personEnabled, ofrVehiclesTouched = length input.ofiVehicles}

recomputeDriverFlagsArm ::
  OnboardingFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  Id DM.Merchant ->
  DP.Person ->
  DocVerificationConfigs ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Maybe Text ->
  Maybe DVC.VehicleCategory ->
  DTC.TransporterConfig ->
  Maybe Bool ->
  Bool ->
  m Bool
recomputeDriverFlagsArm merchantOpCityId merchantId person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory driverName onboardingVehicleCategory transporterConfig mbIsFleetDriver useUnifiedOnboardingFlagsRecompute = do
  driverInfo <- DIQuery.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  let effectiveOnboardingAs = fromMaybe DI.INDIVIDUAL (driverInfo.onboardingAs <|> transporterConfig.defaultOnboardingAs)
      isFleetDriver = fromMaybe (effectiveOnboardingAs == DI.FLEET_DRIVER) mbIsFleetDriver
      allMandatoryDocsValid = checkAllDriverDocsValid' ForVerified (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      allEnablingDocsValid = checkAllDriverDocsValid' ForEnabling (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      derivedApproved = computeApprovedFromDocs (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments
      newApproved =
        if useUnifiedOnboardingFlagsRecompute
          then
            ( case derivedApproved of
                Just True | not allMandatoryDocsValid -> Nothing
                other -> other
            )
          else if allMandatoryDocsValid then Nothing else Just False -- Keeping this for now so that MSIL works, should not be needed but will see later :)
  when (allMandatoryDocsValid /= driverInfo.verified || (useUnifiedOnboardingFlagsRecompute && newApproved /= driverInfo.approved)) $
    DIQueryExtra.updateVerifiedAndApprovedState (cast person.id) allMandatoryDocsValid newApproved
  vehicleGateOk <-
    if not useUnifiedOnboardingFlagsRecompute || transporterConfig.disableDriverWhenUnlinkingVehicle == Just False
      then pure True
      else
        if isFleetDriver || transporterConfig.disableDriverWhenUnlinkingVehicle == Just True
          then isJust <$> DRAQuery.findActiveAssociationByDriver (cast person.id) True
          else pure True
  consentGateOk <-
    if useUnifiedOnboardingFlagsRecompute && effectiveOnboardingAs == DI.FLEET_DRIVER
      then hasActiveFleetAssociation person.id
      else pure True
  -- Fleet disablement is *pulled*, not pushed: a fleet owner going disabled only stamps its own
  -- FleetOwnerInformation. A FLEET_DRIVER picks that up here, on its own recompute, and drops it
  -- again once the fleet's flag clears. Drivers disabled for another reason are left alone.
  effectiveDisabledReasonFlag <-
    if useUnifiedOnboardingFlagsRecompute
      then
        if effectiveOnboardingAs == DI.FLEET_DRIVER
          then do
            fleetDisabled <- isFleetOfDriverDisabled person.id
            pure $ case (fleetDisabled, driverInfo.disabledReasonFlag) of
              (True, Nothing) -> Just DI.FleetDisabled
              (False, Just DI.FleetDisabled) -> Nothing
              (_, existing) -> existing
          else pure $ case driverInfo.disabledReasonFlag of
            Just DI.FleetDisabled -> Nothing
            existing -> existing
      else pure driverInfo.disabledReasonFlag
  when (useUnifiedOnboardingFlagsRecompute && effectiveDisabledReasonFlag /= driverInfo.disabledReasonFlag) $
    DIQuery.updateDisabledReasonFlag effectiveDisabledReasonFlag (cast person.id)
  let explicitlyDisabled = useUnifiedOnboardingFlagsRecompute && isJust effectiveDisabledReasonFlag
      approvedGateOk = if useUnifiedOnboardingFlagsRecompute then newApproved == Just True else driverInfo.approved == Just True
      shouldEnable = not explicitlyDisabled && vehicleGateOk && consentGateOk && allMandatoryDocsValid && allEnablingDocsValid && approvedGateOk
  let justEnabled = shouldEnable && not driverInfo.enabled
  -- The association is read once and reused for both the onboardingAs reconciliation and the
  -- fleet-scoped counter key.
  mbFleetAssoc <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER || useUnifiedOnboardingFlagsRecompute
      then QFDA.findByDriverId person.id True
      else pure Nothing
  -- onboardingAs is derived, not an input: fleet membership is the source of truth, so a driver
  -- holding an active fleet association is a FLEET_DRIVER and anyone else is INDIVIDUAL. Settled
  -- on every recompute, not only on the enable transition, so an association change on an already
  -- enabled or still disabled driver is picked up too.
  let settledOnboardingAs = if isJust mbFleetAssoc then DI.FLEET_DRIVER else DI.INDIVIDUAL
  when (useUnifiedOnboardingFlagsRecompute && driverInfo.onboardingAs /= Just settledOnboardingAs) $
    DIQueryExtra.updateOnboardingAs (Just settledOnboardingAs) (cast person.id)
  if justEnabled
    then do
      enableDriver merchantOpCityId person.id person.role driverName transporterConfig merchantId allMandatoryDocsValid
      whenJust onboardingVehicleCategory $ \category ->
        DIIQuery.updateOnboardingVehicleCategory (Just category) person.id
    else
      when (not shouldEnable && driverInfo.enabled) $
        SDO.disableDriverWithAnalytics merchantOpCityId person.id Nothing
  let mbFleetOwnerId = if effectiveOnboardingAs == DI.FLEET_DRIVER then (.fleetOwnerId) <$> mbFleetAssoc else Nothing
  -- docsVerificationStatus is derived from the same documents as the flags and is written on every
  -- recompute, unified or not. It is the column the UI APIs and the legacy ClickHouse status counts
  -- still read, so letting the unified flow move verified / approved without it would leave those
  -- surfaces showing a stale status.
  let newDocsVerificationStatus = Just $ computeAdminDocsVerificationStatus driverDocuments
  when (newDocsVerificationStatus /= driverInfo.docsVerificationStatus) $
    DIQueryExtra.updateDocsVerificationStatus newDocsVerificationStatus (cast person.id)
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterDriver
    merchantOpCityId
    mbFleetOwnerId
    (bucketsOfFlags' driverInfo.verified driverInfo.approved driverInfo.enabled driverInfo.blocked (isJust driverInfo.disabledReasonFlag))
    (bucketsOfFlags' allMandatoryDocsValid newApproved shouldEnable driverInfo.blocked (isJust effectiveDisabledReasonFlag))
  pure shouldEnable

-- | Fleet owner (not fleet drivers — they go through recomputeDriverVerifiedAndEnabled). Recompute
--   verified/enabled from doc validity, both directions.
--     verified = all isMandatory fleet docs VALID            (excludes OperatorPartnerCode)
--     enabled  = all isMandatoryForEnabling fleet docs VALID (incl. OperatorPartnerCode — the BOT-set enable gate)
--   `approved` is BOT-owned: downgrading verified revokes it. Under enableBotFlow there is NO driver
--   cascade — flags are written directly.
recomputeFleetFlagsArm ::
  OnboardingFlow m r =>
  DP.Person ->
  DocVerificationConfigs ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool ->
  m Bool
recomputeFleetFlagsArm person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory useUnifiedOnboardingFlagsRecompute = do
  fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
  let allFleetMandatoryDocsValid = checkAllDriverDocsValidForVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      allFleetEnablingDocsValid = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      derivedApproved = computeApprovedFromDocs Nothing allDocVerificationConfigs person.role driverDocuments
      newApproved =
        if useUnifiedOnboardingFlagsRecompute
          then derivedApproved
          else if allFleetMandatoryDocsValid then Nothing else Just False -- Keeping this for now so that MSIL works, should not be needed but will see later :)
  when (allFleetMandatoryDocsValid /= fleetOwnerInfo.verified || (useUnifiedOnboardingFlagsRecompute && newApproved /= fleetOwnerInfo.approved)) $
    QFOI.updateFleetOwnerVerifiedAndApprovedStatus allFleetMandatoryDocsValid newApproved person.id
  let explicitlyDisabled = useUnifiedOnboardingFlagsRecompute && isJust fleetOwnerInfo.disabledReasonFlag
      approvedGateOk = if useUnifiedOnboardingFlagsRecompute then newApproved == Just True else True
      newEnabled = not explicitlyDisabled && allFleetEnablingDocsValid && approvedGateOk
  when (newEnabled /= fleetOwnerInfo.enabled) $
    QFOI.updateFleetOwnerEnabledStatus newEnabled person.id
  -- docsVerificationStatus is derived from the same documents as the flags and is written on every
  -- recompute, unified or not. It is the column the UI APIs and the legacy ClickHouse status counts
  -- still read, so letting the unified flow move verified / approved without it would leave those
  -- surfaces showing a stale status.
  let newDocsVerificationStatus = Just $ computeAdminDocsVerificationStatus driverDocuments
  when (newDocsVerificationStatus /= fleetOwnerInfo.docsVerificationStatus) $
    QFOI.updateDocsVerificationStatus newDocsVerificationStatus person.id
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterFleetOwner
    person.merchantOperatingCityId
    (Just person.id.getId)
    (bucketsOfFlags' fleetOwnerInfo.verified fleetOwnerInfo.approved fleetOwnerInfo.enabled fleetOwnerInfo.blocked (isJust fleetOwnerInfo.disabledReasonFlag))
    (bucketsOfFlags' allFleetMandatoryDocsValid newApproved newEnabled fleetOwnerInfo.blocked (isJust fleetOwnerInfo.disabledReasonFlag))
  pure newEnabled

recomputeVehicleFlagsArm ::
  OnboardingFlow m r =>
  Text ->
  VehicleDocumentItem ->
  [DVC.DocumentVerificationConfig] ->
  Maybe Bool ->
  Bool ->
  m ()
recomputeVehicleFlagsArm registrationNo vehicleDocItem allDocumentVerificationConfigs makeSelfieAadhaarPanMandatory useUnifiedOnboardingFlagsRecompute = do
  let vehicleDocItem' = vehicleDocItem
      allVehicleMandatoryDocsValid = checkAllVehicleDocsValidForVerified allDocumentVerificationConfigs vehicleDocItem' makeSelfieAadhaarPanMandatory
  rcHash <- getDbHash registrationNo
  -- docsVerificationStatus is derived from the same documents as the flags and is written on every
  -- recompute, unified or not. It is the column the UI APIs and the legacy ClickHouse status counts
  -- still read, so letting the unified flow move verified / approved without it would leave those
  -- surfaces showing a stale status.
  RCQuery.updateDocsVerificationStatusByCertificateNumberHash (Just $ computeAdminDocsVerificationStatus vehicleDocItem'.documents) rcHash
  if useUnifiedOnboardingFlagsRecompute
    then do
      mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
      let derivedApproved = computeApprovedFromDocs Nothing (Right allDocumentVerificationConfigs) DP.DRIVER vehicleDocItem'.documents
      whenJust mbRc $ \rc -> do
        let newVerified = Just allVehicleMandatoryDocsValid
            newApproved = if allVehicleMandatoryDocsValid then derivedApproved else Just False
        when (newVerified /= rc.verified || newApproved /= rc.approved) $
          VRCEQuery.updateApprovedAndVerifiedById newApproved newVerified rc.id
        -- A vehicle has no `enabled` flag, so that bucket is always False on both sides.
        whenJust rc.merchantOperatingCityId $ \rcMerchantOpCityId ->
          adjustOnboardingCounters
            useUnifiedOnboardingFlagsRecompute
            CounterVehicle
            rcMerchantOpCityId
            rc.fleetOwnerId
            (bucketsOfFlags (fromMaybe False rc.verified) rc.approved False)
            (bucketsOfFlags allVehicleMandatoryDocsValid newApproved False)
    else RCQuery.updateVerifiedByCertificateNumberHash (Just allVehicleMandatoryDocsValid) rcHash

-- | Which entity's onboarding counters a write belongs to.
data OnboardingCounterEntity = CounterDriver | CounterFleetOwner | CounterVehicle

counterEntityTag :: OnboardingCounterEntity -> Text
counterEntityTag = \case
  CounterDriver -> "driver"
  CounterFleetOwner -> "fleet"
  CounterVehicle -> "vehicle"

-- | Bucket membership derived from the onboarding flags, matching the summary row above the
--   onboarding grids. The buckets deliberately overlap -- `enabled` is a subset of `approved` --
--   so each is its own counter rather than a slot in a single bucket.
data OnboardingBuckets = OnboardingBuckets
  { obVerified :: Bool,
    obApproved :: Bool,
    obPending :: Bool,
    obRejected :: Bool,
    obEnabled :: Bool,
    obBlocked :: Bool,
    obDisabled :: Bool
  }

bucketsOfFlags :: Bool -> Maybe Bool -> Bool -> OnboardingBuckets
bucketsOfFlags verified approved enabled = (bucketsOfFlags' verified approved enabled False False)

-- | Full bucket set, including the two post-onboarding states. `blocked` and `disabled` are
--   independent of the document-derived buckets, so an entity can be counted in both.
bucketsOfFlags' :: Bool -> Maybe Bool -> Bool -> Bool -> Bool -> OnboardingBuckets
bucketsOfFlags' verified approved enabled blocked disabled =
  OnboardingBuckets
    { obVerified = verified,
      obApproved = verified && approved == Just True,
      obPending = isNothing approved,
      obRejected = not verified && approved == Just False,
      obEnabled = verified && approved == Just True && enabled,
      obBlocked = blocked,
      obDisabled = disabled
    }

mkOnboardingCounterKey :: OnboardingCounterEntity -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Text -> Text
mkOnboardingCounterKey entity merchantOpCityId mbFleetOwnerId bucket =
  "Onboarding:Counts:" <> counterEntityTag entity <> ":" <> merchantOpCityId.getId
    <> maybe "" (":" <>) mbFleetOwnerId
    <> ":"
    <> bucket

-- | Move the onboarding counters from one flag state to the next, incrementing the buckets the
--   entity entered and decrementing the ones it left. Buckets whose membership did not change are
--   not touched, so an idempotent recompute writes nothing.
--
--   Maintained only under unifiedOnboardingFlagsRecompute: that is the flow where these flags are
--   the source of truth. Note the counters therefore describe the *flag* state; the existing
--   statusSummary endpoint still counts docsVerificationStatus in ClickHouse, so the two do not
--   agree until that read path is switched over (and gains a rebuild-on-miss fallback).
adjustOnboardingCounters ::
  OnboardingFlow m r =>
  Bool ->
  OnboardingCounterEntity ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  OnboardingBuckets ->
  OnboardingBuckets ->
  m ()
adjustOnboardingCounters useUnifiedOnboardingFlagsRecompute entity merchantOpCityId mbFleetOwnerId old new =
  when useUnifiedOnboardingFlagsRecompute $
    forM_ changed $ \(bucket, delta) -> do
      let scopedKeys = mkOnboardingCounterKey entity merchantOpCityId Nothing bucket : maybe [] (\fleetOwnerId -> [mkOnboardingCounterKey entity merchantOpCityId (Just fleetOwnerId) bucket]) mbFleetOwnerId
      forM_ scopedKeys $ \key -> void $ Hedis.incrby key delta
  where
    step o n
      | o == n = 0
      | n = 1
      | otherwise = -1
    changed =
      filter ((/= 0) . snd) $
        [ ("verified", step old.obVerified new.obVerified),
          ("approved", step old.obApproved new.obApproved),
          ("pending", step old.obPending new.obPending),
          ("rejected", step old.obRejected new.obRejected),
          ("enabled", step old.obEnabled new.obEnabled),
          ("blocked", step old.obBlocked new.obBlocked),
          ("disabled", step old.obDisabled new.obDisabled)
        ]

data BlockChange
  = Block BlockPayload
  | Unblock SimplePayload
  | SimpleBlock SimplePayload
  deriving (Show, Generic)

data BlockPayload = BlockPayload
  { bpReason :: Maybe Text,
    bpExpiryHours :: Maybe Int,
    bpDashboardUserName :: Text,
    bpMerchantId :: Id DM.Merchant,
    bpReasonCode :: Text,
    bpMerchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    bpBlockedBy :: DTDBT.BlockedBy,
    bpActive :: Maybe Bool,
    bpMode :: Maybe Common.DriverMode,
    bpFlag :: BlockReasonFlag
  }
  deriving (Show, Generic)

data SimplePayload = SimplePayload
  { spModifier :: Maybe Text,
    spMerchantId :: Id DM.Merchant,
    spMerchantOperatingCityId :: Id DMOC.MerchantOperatingCity,
    spBlockedBy :: DTDBT.BlockedBy
  }
  deriving (Show, Generic)

markBlockFlags ::
  (MonadFlow m, EsqDBFlow m r, CacheFlow m r, Hedis.HedisFlow m r, Hedis.HedisLTSFlowEnv r) =>
  Id DP.Person ->
  BlockChange ->
  m ()
markBlockFlags personId = \case
  Block p ->
    DIQueryExtra.updateDynamicBlockedStateWithActivity (cast personId) p.bpReason p.bpExpiryHours p.bpDashboardUserName p.bpMerchantId p.bpReasonCode p.bpMerchantOperatingCityId p.bpBlockedBy True p.bpActive p.bpMode p.bpFlag
  Unblock p ->
    DIQueryExtra.updateBlockedState (cast personId) False p.spModifier p.spMerchantId p.spMerchantOperatingCityId p.spBlockedBy
  SimpleBlock p ->
    DIQueryExtra.updateBlockedState (cast personId) True p.spModifier p.spMerchantId p.spMerchantOperatingCityId p.spBlockedBy

-- | A change to an entity's disabled state. Separate from the block path because deriving
--   `enabled` needs the full onboarding effect set, while blocking does not.
data DisabledChange
  = AdminDisable DI.DisabledReasonFlag
  | AdminEnable
  | FleetRejectionDisable

-- | Write an entity's disabled state. Under unified only `disabledReasonFlag` is written --
--   `enabled` is derived from documents plus that flag by recomputeOnboardingFlags, so the caller
--   refreshes afterwards. The legacy branch keeps the historical direct writes.
markDisabledFlags :: OnboardingFlow m r => Bool -> DP.Person -> DisabledChange -> m ()
markDisabledFlags unified person change = case change of
  AdminDisable reason ->
    if SDO.isFleetRole person.role
      then do
        unless unified $ QFOI.updateFleetOwnerEnabledStatus False person.id
        QFOI.updateFleetOwnerDisabledReasonFlag (Just reason) person.id
      else do
        unless unified $ SDO.disableDriverWithAnalytics person.merchantOperatingCityId (cast person.id) Nothing
        DIQuery.updateDisabledReasonFlag (Just reason) (cast person.id)
  AdminEnable ->
    if SDO.isFleetRole person.role
      then QFOI.updateFleetOwnerDisabledReasonFlag Nothing person.id
      else DIQuery.updateDisabledReasonFlag Nothing (cast person.id)
  FleetRejectionDisable -> do
    fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
    when fleetOwnerInfo.enabled $ do
      unless unified $ QFOI.updateFleetOwnerEnabledStatus False person.id
      -- Stamp the fleet only; its drivers pick this up on their own recompute.
      QFOI.updateFleetOwnerDisabledReasonFlag (Just DI.FleetDisabled) person.id

-- | The counts a summary row needs. `total` is deliberately not one of the delta-maintained
--   buckets: the buckets overlap (enabled is a subset of approved) so they cannot be summed, and
--   a recompute observes flag transitions, never entity creation -- so no delta can express a
--   running total. Seeding it alongside the others would freeze it at rebuild time. It is counted
--   directly instead and cached briefly.
data OnboardingCounts = OnboardingCounts
  { ocTotal :: Int,
    ocVerified :: Int,
    ocApproved :: Int,
    ocPending :: Int,
    ocRejected :: Int,
    ocEnabled :: Int,
    ocBlocked :: Int,
    ocDisabled :: Int
  }

readOnboardingCounts ::
  OnboardingFlow m r =>
  OnboardingCounterEntity ->
  Id DMOC.MerchantOperatingCity ->
  Maybe Text ->
  m OnboardingCounts
readOnboardingCounts entity merchantOpCityId mbFleetOwnerId = do
  verified <- readBucket "verified"
  approved <- readBucket "approved"
  pending <- readBucket "pending"
  rejected <- readBucket "rejected"
  enabled <- readBucket "enabled"
  blocked <- readBucket "blocked"
  disabled <- readBucket "disabled"
  pure $
    OnboardingCounts
      { ocTotal = verified + approved + pending + rejected,
        ocVerified = verified,
        ocApproved = approved,
        ocPending = pending,
        ocRejected = rejected,
        ocEnabled = enabled,
        ocBlocked = blocked,
        ocDisabled = disabled
      }
  where
    keyFor = mkOnboardingCounterKey entity merchantOpCityId mbFleetOwnerId
    readBucket bucket = fromMaybe 0 <$> Hedis.get @Int (keyFor bucket)
