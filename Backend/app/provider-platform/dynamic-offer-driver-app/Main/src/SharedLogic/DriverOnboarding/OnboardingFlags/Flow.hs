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
    bucketsOfFlags,
    OnboardingCounts (..),
    readOnboardingCounts,
    removeDriverFromOnboardingCounters,
    removeFleetOwnerFromOnboardingCounters,
    adjustDriverOnboardingCounters,
    adjustFleetOnboardingCounters,
    adjustVehicleOnboardingCounters,
    BlockChange (..),
    BlockPayload (..),
    SimplePayload (..),
    markBlockFlags,
    DisabledChange (..),
    markEnableDisableReasonFlags,
    PersonFlagsCtx (..),
    VehicleDocsEntry (..),
  )
where

import Control.Applicative ((<|>))
import Data.List (nub)
import qualified Domain.Types.Common as Common
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import Kernel.External.Encryption (getDbHash)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.ApprovalComms as ApprovalComms
import SharedLogic.DriverOnboarding.Common
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Checks as SGuard
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverInformationExtra as DIQueryExtra
import qualified Storage.Queries.DriverRCAssociationExtra as DAQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.FleetOwnerInformationExtra as QFOIExtra
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Storage.Queries.VehicleRegistrationCertificateExtra as VRCEQuery
import Tools.Error (BlockReasonFlag (..))

data VehicleDocsEntry = VehicleDocsEntry
  { vdeRegistrationNo :: Text,
    vdeItem :: VehicleDocumentItem,
    vdeConfigs :: [DVC.DocumentVerificationConfig],
    -- | The caller's value when known (statusHandler); standalone recomputes pass Nothing.
    vdeMakeSelfieAadhaarPanMandatory :: Maybe Bool,
    -- | The RC's city has enableBotFlow: `approved` is BOT-owned.
    vdeEnableBotFlow :: Bool
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
    pfcIsFleetDriver :: Maybe Bool,
    pfcVehicleDocs :: [VehicleDocumentItem]
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
              pfc.pfcTransporterConfig
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
              pfc.pfcVehicleDocs
              useUnifiedOnboardingFlagsRecompute
  forM_ input.ofiVehicles $ \entry ->
    recomputeVehicleFlagsArm entry.vdeRegistrationNo entry.vdeItem entry.vdeConfigs entry.vdeMakeSelfieAadhaarPanMandatory entry.vdeEnableBotFlow useUnifiedOnboardingFlagsRecompute
  pure OnboardingFlagsResult {ofrPersonEnabled = personEnabled, ofrVehiclesTouched = length input.ofiVehicles}

mkUnavailableDoc :: DVC.DocumentType -> DocumentStatusItem
mkUnavailableDoc docType =
  DocumentStatusItem
    { documentType = docType,
      documentId = Nothing,
      verificationStatus = NO_DOC_AVAILABLE,
      verificationMessage = Nothing,
      verificationUrl = Nothing,
      s3Path = Nothing,
      imageId = Nothing,
      imageId2 = Nothing,
      documentExpiry = Nothing,
      metadata = Nothing,
      commonDocumentData = Nothing
    }

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
  [VehicleDocumentItem] ->
  Bool ->
  m Bool
recomputeDriverFlagsArm merchantOpCityId merchantId person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory driverName onboardingVehicleCategory transporterConfig mbIsFleetDriver vehicleDocuments useUnifiedOnboardingFlagsRecompute = do
  driverInfo <- DIQuery.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  mutationAllowed <-
    withTryCatch "onboardingFlags:mutation" (SGuard.guardOnboardingAction transporterConfig SGuard.None SGuard.OnboardingFlagMutation (SGuard.TargetDriver person.id)) >>= \case
      Right () -> pure True
      Left err -> do
        logError $ "onboardingFlags: mutation blocked for " <> person.id.getId <> ": " <> show err
        pure False
  let effectiveOnboardingAs = fromMaybe DI.INDIVIDUAL (driverInfo.onboardingAs <|> transporterConfig.defaultOnboardingAs)
      approvalSupported = approvalSupportedInConfigs allDocVerificationConfigs
  isFleetDriver <- case mbIsFleetDriver of
    Just isFleet -> pure isFleet
    Nothing
      | effectiveOnboardingAs == DI.FLEET_DRIVER ->
        if approvalSupported
          then pure True
          else isJust <$> QFDA.findOneByDriverIdWithStatus person.id
      | otherwise -> pure False
  let driverDocConfigs = case allDocVerificationConfigs of Right configs -> configs; Left _ -> []
      unavailableVehicleDocs =
        map mkUnavailableDoc . nub . map (.documentType) $
          filter (\config -> config.vehicleCategory == vehicleCategory && isVehicleSideDocType config.documentCategory config.documentType) driverDocConfigs
      checkDriverDocs mode = checkAllDocsValid mode (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      checkVehicleDocs mode category docs = checkAllDocsValid mode (Just isFleetDriver) (Right driverDocConfigs) DP.DRIVER docs category makeSelfieAadhaarPanMandatory
      mbValidVehicleDoc mode = find (\item -> checkVehicleDocs mode (vehicleDocCategory item) item.documents) vehicleDocuments
      includeVehicleDocs = transporterConfig.separateDriverVehicleEnablement /= Just True
      vehicleDocsOk mode =
        not includeVehicleDocs
          || ( case vehicleDocuments of
                 [] -> checkVehicleDocs mode vehicleCategory unavailableVehicleDocs
                 _ -> isJust (mbValidVehicleDoc mode)
             )
      allMandatoryDocsValid = checkDriverDocs ForVerified && vehicleDocsOk ForVerified
      allEnablingDocsValid = checkDriverDocs ForEnabling && vehicleDocsOk ForEnabling
      approvalDocs =
        if includeVehicleDocs
          then driverDocuments <> maybe unavailableVehicleDocs (.documents) (mbValidVehicleDoc ForEnabling)
          else driverDocuments
      derivedApproved = computeApprovedFromDocs (Just isFleetDriver) allDocVerificationConfigs person.role approvalDocs
      newApproved
        | transporterConfig.enableBotFlow == Just True = botOwnedApproved
        | otherwise = case derivedApproved of
          Just True | not allMandatoryDocsValid -> Nothing
          other -> other
      -- BOT owns `approved`: only the BOT review sets it True. Losing `verified` revokes it (re-approval
      -- required), regaining `verified` returns the driver to review, and a rejected approval doc on a
      -- verified driver revokes it. Disabling revokes it too (via the disable helper, counted below).
      botOwnedApproved
        | driverInfo.verified && not verifiedToWrite = Just False
        | not driverInfo.verified && verifiedToWrite = Nothing
        | verifiedToWrite && derivedApproved == Just False = Just False
        | otherwise = driverInfo.approved
      holdEnabledWithoutDocsVerifiedEnabledOrApproved = not approvalSupported && (driverInfo.verified || driverInfo.enabled)
      verifiedToWrite =
        if holdEnabledWithoutDocsVerifiedEnabledOrApproved || (driverInfo.verified && not allMandatoryDocsValid && not mutationAllowed)
          then driverInfo.verified
          else allMandatoryDocsValid
      approvedToWrite =
        if holdEnabledWithoutDocsVerifiedEnabledOrApproved || (driverInfo.approved == Just True && newApproved /= Just True && not mutationAllowed)
          then driverInfo.approved
          else newApproved
  when (verifiedToWrite /= driverInfo.verified || approvedToWrite /= driverInfo.approved) $
    DIQueryExtra.updateVerifiedAndApprovedState (cast person.id) verifiedToWrite approvedToWrite
  when (approvalSupported && driverInfo.approved /= Just True && approvedToWrite == Just True) $
    ApprovalComms.notifyPersonApproved merchantOpCityId person
  consentGateOk <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER
      then hasActiveFleetAssociation person.id
      else pure True
  -- Fleet disablement is *pulled*, not pushed: a fleet owner going disabled only stamps its own
  -- FleetOwnerInformation. A FLEET_DRIVER picks that up here, on its own recompute, and drops it
  -- again once the fleet's flag clears. Drivers disabled for another reason are left alone.
  effectiveDisabledReasonFlag <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER && consentGateOk
      then do
        fleetDisabled <- isFleetOfDriverDisabled person.id
        pure $ case (fleetDisabled, driverInfo.disabledReasonFlag) of
          (True, Nothing) -> Just DI.FleetDisabled
          (False, Just DI.FleetDisabled) -> Nothing
          (_, existing) -> existing
      else pure $ case driverInfo.disabledReasonFlag of
        Just DI.FleetDisabled -> Nothing
        existing -> existing
  when (effectiveDisabledReasonFlag /= driverInfo.disabledReasonFlag) $
    DIQuery.updateDisabledReasonFlag effectiveDisabledReasonFlag (cast person.id)
  -- A disabled driver keeps its force-enable marker unless the city wants the disable to send them
  -- back to onboarding; clearing it is what makes `enabled` derive from documents again.
  effectiveEnabledReasonFlag <-
    if holdEnabledWithoutDocsVerifiedEnabledOrApproved
      then do
        unless (driverInfo.enabledReasonFlag == Just DI.AdminEnabled) $
          DIQueryExtra.updateEnabledReasonFlag (Just DI.AdminEnabled) (cast person.id)
        pure (Just DI.AdminEnabled)
      else
        if mutationAllowed
          && isJust effectiveDisabledReasonFlag
          && driverInfo.enabledReasonFlag == Just DI.AdminEnabled
          && transporterConfig.forceEnabledBypassingDocsUponDisableTakesToOnboarding == Just True
          then do
            DIQueryExtra.updateEnabledReasonFlag Nothing (cast person.id)
            pure Nothing
          else pure driverInfo.enabledReasonFlag
  let approvedGateOk = approvedToWrite == Just True
      bypassDocGates = effectiveEnabledReasonFlag == Just DI.AdminEnabled
      docsDerivedEnable = consentGateOk && verifiedToWrite && allEnablingDocsValid && approvedGateOk
      derivedShouldEnable = bypassDocGates || docsDerivedEnable
      shouldEnable = derivedShouldEnable || (driverInfo.enabled && not mutationAllowed)
  when (bypassDocGates && docsDerivedEnable) $
    DIQueryExtra.updateEnabledReasonFlag Nothing (cast person.id)
  let justEnabled = shouldEnable && not driverInfo.enabled
      justDisabled = not shouldEnable && driverInfo.enabled
      -- Under BOT updateEnabledVerifiedStateWithAnalytics also revokes `approved` when it writes enabled = False
      -- (disable) or verified = False (an admin force-enable of an unverified driver), so the counters must see
      -- the revoked value, not the one written above.
      approvedAfterWrites =
        if transporterConfig.enableBotFlow == Just True && (justDisabled || (justEnabled && not verifiedToWrite))
          then Just False
          else approvedToWrite
  -- The association is read once and reused for both the onboardingAs reconciliation and the
  -- fleet-scoped counter key.
  mbFleetAssoc <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER
      then QFDA.findOneByDriverIdWithStatus person.id
      else pure Nothing
  if justEnabled
    then do
      -- Under enableBotFlow RC activation stays manual: no auto-activation on enabling.
      let mbRcNumberToActivate = if transporterConfig.enableBotFlow == Just True then Nothing else (.registrationNo) <$> mbValidVehicleDoc ForEnabling
      enableDriver merchantOpCityId person.id person.role driverName transporterConfig merchantId verifiedToWrite mbRcNumberToActivate
      whenJust onboardingVehicleCategory $ \category ->
        DIIQuery.updateOnboardingVehicleCategory (Just category) person.id
    else
      when justDisabled $
        SDO.disableDriverWithAnalytics merchantOpCityId person.id Nothing
  let mbFleetOwnerId = if effectiveOnboardingAs == DI.FLEET_DRIVER then (.fleetOwnerId) <$> mbFleetAssoc else Nothing
  when (useUnifiedOnboardingFlagsRecompute && driverInfo.isNew /= Just False) $ DIQuery.updateIsNew (Just False) (cast person.id)
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterDriver
    merchantOpCityId
    mbFleetOwnerId
    (asAlreadyCounted driverInfo.isNew $ bucketsOfFlags' driverInfo.verified driverInfo.approved driverInfo.enabled driverInfo.blocked (isJust driverInfo.disabledReasonFlag))
    (bucketsOfFlags' verifiedToWrite approvedAfterWrites shouldEnable driverInfo.blocked (isJust effectiveDisabledReasonFlag))
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
  DTC.TransporterConfig ->
  Bool ->
  m Bool
recomputeFleetFlagsArm person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory transporterConfig useUnifiedOnboardingFlagsRecompute = do
  fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
  let botFlow = transporterConfig.enableBotFlow == Just True
      allFleetMandatoryDocsValid = checkAllDriverDocsValidForVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      allFleetEnablingDocsValid = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      derivedApproved = computeApprovedFromDocs Nothing allDocVerificationConfigs person.role driverDocuments
      approvalSupported = approvalSupportedInConfigs allDocVerificationConfigs
      newApproved
        | botFlow = botOwnedApproved
        | otherwise = case derivedApproved of
          Just True | not allFleetMandatoryDocsValid -> Nothing
          other -> other
      -- BOT owns `approved`, as on the driver side: only the BOT review sets it True; losing `verified`
      -- revokes it, regaining `verified` returns the fleet to review, a rejected approval doc on a verified
      -- fleet revokes it, and losing `enabled` revokes it (below).
      botOwnedApproved
        | fleetOwnerInfo.verified && not verifiedToWrite = Just False
        | not fleetOwnerInfo.verified && verifiedToWrite = Nothing
        | verifiedToWrite && derivedApproved == Just False = Just False
        | otherwise = fleetOwnerInfo.approved
      -- BOT cities recompute fleet flags in both directions; the hold keeps non-BOT cities without
      -- approval support on their sticky flags.
      holdEnabledWithoutDocsVerifiedEnabledOrApproved = not botFlow && not approvalSupported && (fleetOwnerInfo.verified || fleetOwnerInfo.enabled)
      verifiedToWrite = if holdEnabledWithoutDocsVerifiedEnabledOrApproved then fleetOwnerInfo.verified else allFleetMandatoryDocsValid
      approvedToWrite = if holdEnabledWithoutDocsVerifiedEnabledOrApproved then fleetOwnerInfo.approved else newApproved
      approvedGateOk = approvedToWrite == Just True
      newEnabled = if holdEnabledWithoutDocsVerifiedEnabledOrApproved then fleetOwnerInfo.enabled else allFleetEnablingDocsValid && approvedGateOk
      approvedAfterWrites = if botFlow && fleetOwnerInfo.enabled && not newEnabled then Just False else approvedToWrite
  when (verifiedToWrite /= fleetOwnerInfo.verified || approvedAfterWrites /= fleetOwnerInfo.approved) $
    QFOI.updateFleetOwnerVerifiedAndApprovedStatus verifiedToWrite approvedAfterWrites person.id
  when (approvalSupported && fleetOwnerInfo.approved /= Just True && approvedAfterWrites == Just True) $
    ApprovalComms.notifyPersonApproved person.merchantOperatingCityId person
  when (newEnabled /= fleetOwnerInfo.enabled) $
    QFOI.updateFleetOwnerEnabledStatus newEnabled person.id
  when (useUnifiedOnboardingFlagsRecompute && fleetOwnerInfo.isNew /= Just False) $ QFOI.updateIsNew (Just False) person.id
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterFleetOwner
    person.merchantOperatingCityId
    (Just person.id.getId)
    (asAlreadyCounted fleetOwnerInfo.isNew $ bucketsOfFlags' fleetOwnerInfo.verified fleetOwnerInfo.approved fleetOwnerInfo.enabled fleetOwnerInfo.blocked (isJust fleetOwnerInfo.disabledReasonFlag))
    (bucketsOfFlags' verifiedToWrite approvedAfterWrites newEnabled fleetOwnerInfo.blocked (isJust fleetOwnerInfo.disabledReasonFlag))
  pure newEnabled

recomputeVehicleFlagsArm ::
  OnboardingFlow m r =>
  Text ->
  VehicleDocumentItem ->
  [DVC.DocumentVerificationConfig] ->
  Maybe Bool ->
  Bool ->
  Bool ->
  m ()
recomputeVehicleFlagsArm registrationNo vehicleDocItem allDocumentVerificationConfigs makeSelfieAadhaarPanMandatory botFlow useUnifiedOnboardingFlagsRecompute = do
  let vehicleDocItem' = vehicleDocItem
      allVehicleMandatoryDocsValid = checkAllVehicleDocsValidForVerified allDocumentVerificationConfigs vehicleDocItem' makeSelfieAadhaarPanMandatory
  rcHash <- getDbHash registrationNo
  mbRc <- RCQuery.findLastVehicleRCWrapper registrationNo
  let derivedApproved = computeApprovedFromDocs Nothing (Right allDocumentVerificationConfigs) DP.DRIVER vehicleDocItem'.documents
      approvalSupported = approvalSupportedInConfigs (Right allDocumentVerificationConfigs)
  case mbRc of
    Just rc -> do
      let holdEnabledWithoutDocsVerifiedEnabledOrApproved = not botFlow && not approvalSupported && rc.verified == Just True
          newVerified = if holdEnabledWithoutDocsVerifiedEnabledOrApproved then rc.verified else Just allVehicleMandatoryDocsValid
          newApproved
            | holdEnabledWithoutDocsVerifiedEnabledOrApproved = rc.approved
            | botFlow = botOwnedApproved
            | otherwise = case derivedApproved of
              Just True | not allVehicleMandatoryDocsValid -> Nothing
              other -> other
          -- BOT owns the RC's `approved`: only the BOT review sets it True. Losing `verified` (True -> False)
          -- revokes it, and so does a rejected approval doc on a verified RC. A new RC (verified NULL) whose
          -- first recompute gives False was never verified, so it stays pending instead of counting as rejected.
          botOwnedApproved
            | rc.verified == Just True && newVerified == Just False = Just False
            | newVerified == Just True && derivedApproved == Just False = Just False
            | otherwise = rc.approved
      when (newVerified /= rc.verified || newApproved /= rc.approved) $
        VRCEQuery.updateApprovedAndVerifiedById newApproved newVerified rc.id
      when (approvalSupported && rc.approved /= Just True && newApproved == Just True) $
        ApprovalComms.notifyVehicleApproved rc registrationNo
      when (useUnifiedOnboardingFlagsRecompute && rc.approved /= Just True && newApproved == Just True) $
        activateRcForSoleLinkedEnabledDriver rc.id registrationNo
      -- A vehicle has no `enabled` flag, so that bucket is always False on both sides.
      when (useUnifiedOnboardingFlagsRecompute && rc.isNew /= Just False) $ RCQuery.updateIsNew (Just False) rc.id
      whenJust rc.merchantOperatingCityId $ \rcMerchantOpCityId ->
        adjustOnboardingCounters
          useUnifiedOnboardingFlagsRecompute
          CounterVehicle
          rcMerchantOpCityId
          rc.fleetOwnerId
          (asAlreadyCounted rc.isNew $ bucketsOfFlags (fromMaybe False rc.verified) rc.approved False)
          (bucketsOfFlags (fromMaybe False newVerified) newApproved False)
    Nothing -> RCQuery.updateVerifiedByCertificateNumberHash (Just allVehicleMandatoryDocsValid) rcHash

activateRcForSoleLinkedEnabledDriver :: OnboardingFlow m r => Id RC.VehicleRegistrationCertificate -> Text -> m ()
activateRcForSoleLinkedEnabledDriver rcId registrationNo =
  DAQuery.findAllActiveAssociationByRCId rcId >>= \case
    [assoc] | not assoc.isRcActive -> do
      mbDriverInfo <- DIQuery.findById (cast assoc.driverId)
      mbVehicle <- QVehicle.findById assoc.driverId
      when (maybe False (.enabled) mbDriverInfo && isNothing mbVehicle) $ do
        mbPerson <- QPerson.findById assoc.driverId
        whenJust mbPerson $ \person -> void $ withTryCatch "activateRCAutomatically:vehicleApproved" (activateRCAutomatically person.id person.merchantId person.merchantOperatingCityId registrationNo)
    _ -> pure ()

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
  { obTotal :: Bool,
    obVerified :: Bool,
    obApproved :: Bool,
    obRejected :: Bool,
    obEnabled :: Bool,
    obBlocked :: Bool,
    obDisabled :: Bool
  }

bucketsOfFlags :: Bool -> Maybe Bool -> Bool -> OnboardingBuckets
bucketsOfFlags verified approved enabled = (bucketsOfFlags' verified approved enabled False False)

asAlreadyCounted :: Maybe Bool -> OnboardingBuckets -> OnboardingBuckets
asAlreadyCounted mbIsNew buckets = buckets {obTotal = mbIsNew == Just False}

-- | Full bucket set, including the two post-onboarding states. `blocked` and `disabled` are
--   independent of the document-derived buckets, so an entity can be counted in both.
bucketsOfFlags' :: Bool -> Maybe Bool -> Bool -> Bool -> Bool -> OnboardingBuckets
bucketsOfFlags' verified approved enabled blocked disabled =
  OnboardingBuckets
    { obTotal = True,
      obVerified = verified && isNothing approved,
      obApproved = approved == Just True,
      obRejected = approved == Just False,
      obEnabled = enabled,
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
--   the source of truth. Elsewhere the statusSummary endpoint buckets the same flags from the
--   ClickHouse mirrors with `bucketsOfFlags`, so both paths describe the flag state.
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
        [ ("total", step old.obTotal new.obTotal),
          ("verified", step old.obVerified new.obVerified),
          ("approved", step old.obApproved new.obApproved),
          ("rejected", step old.obRejected new.obRejected),
          ("enabled", step old.obEnabled new.obEnabled),
          ("blocked", step old.obBlocked new.obBlocked),
          ("disabled", step old.obDisabled new.obDisabled)
        ]

emptyOnboardingBuckets :: OnboardingBuckets
emptyOnboardingBuckets =
  OnboardingBuckets
    { obTotal = False,
      obVerified = False,
      obApproved = False,
      obRejected = False,
      obEnabled = False,
      obBlocked = False,
      obDisabled = False
    }

-- | Drop a permanently deleted driver out of every counter it currently occupies, `total`
--   included. Mirrors the bucket derivation and the key scoping of 'recomputeDriverFlagsArm' so
--   the decrements land on exactly the keys that recompute incremented. An entity that never went
--   through a recompute (`isNew` still unset) was never counted into `total`, and
--   'asAlreadyCounted' keeps it out of the decrement too.
removeDriverFromOnboardingCounters :: OnboardingFlow m r => Bool -> DTC.TransporterConfig -> DP.Person -> DI.DriverInformation -> m ()
removeDriverFromOnboardingCounters useUnifiedOnboardingFlagsRecompute transporterConfig person driverInfo = do
  let effectiveOnboardingAs = fromMaybe DI.INDIVIDUAL (driverInfo.onboardingAs <|> transporterConfig.defaultOnboardingAs)
  mbFleetAssoc <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER
      then QFDA.findOneByDriverIdWithStatus person.id
      else pure Nothing
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterDriver
    person.merchantOperatingCityId
    ((.fleetOwnerId) <$> mbFleetAssoc)
    (asAlreadyCounted driverInfo.isNew $ bucketsOfFlags' driverInfo.verified driverInfo.approved driverInfo.enabled driverInfo.blocked (isJust driverInfo.disabledReasonFlag))
    emptyOnboardingBuckets

-- | 'removeDriverFromOnboardingCounters' for a fleet owner.
removeFleetOwnerFromOnboardingCounters :: OnboardingFlow m r => Bool -> DP.Person -> DFOI.FleetOwnerInformation -> m ()
removeFleetOwnerFromOnboardingCounters useUnifiedOnboardingFlagsRecompute person fleetOwnerInfo =
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterFleetOwner
    person.merchantOperatingCityId
    (Just person.id.getId)
    (asAlreadyCounted fleetOwnerInfo.isNew $ bucketsOfFlags' fleetOwnerInfo.verified fleetOwnerInfo.approved fleetOwnerInfo.enabled fleetOwnerInfo.blocked (isJust fleetOwnerInfo.disabledReasonFlag))
    emptyOnboardingBuckets

-- | Move a driver's counters for a flag write made outside 'recomputeDriverFlagsArm' (the BOT
--   review). The arm moves the counters from the row it reads, so a write it never sees would stay
--   uncounted for good; this applies the delta the arm would have, with the same bucket derivation,
--   key scoping and first-count handling.
adjustDriverOnboardingCounters :: OnboardingFlow m r => DTC.TransporterConfig -> DP.Person -> DI.DriverInformation -> DI.DriverInformation -> m ()
adjustDriverOnboardingCounters transporterConfig person oldInfo newInfo = do
  let useUnifiedOnboardingFlagsRecompute = transporterConfig.unifiedOnboardingFlagsRecompute == Just True
      effectiveOnboardingAs = fromMaybe DI.INDIVIDUAL (oldInfo.onboardingAs <|> transporterConfig.defaultOnboardingAs)
      bucketsOf info = bucketsOfFlags' info.verified info.approved info.enabled info.blocked (isJust info.disabledReasonFlag)
  mbFleetAssoc <-
    if effectiveOnboardingAs == DI.FLEET_DRIVER
      then QFDA.findOneByDriverIdWithStatus person.id
      else pure Nothing
  when (useUnifiedOnboardingFlagsRecompute && oldInfo.isNew /= Just False) $ DIQuery.updateIsNew (Just False) (cast person.id)
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterDriver
    person.merchantOperatingCityId
    ((.fleetOwnerId) <$> mbFleetAssoc)
    (asAlreadyCounted oldInfo.isNew $ bucketsOf oldInfo)
    (bucketsOf newInfo)

-- | 'adjustDriverOnboardingCounters' for a vehicle, mirroring 'recomputeVehicleFlagsArm'.
adjustVehicleOnboardingCounters :: OnboardingFlow m r => DTC.TransporterConfig -> RC.VehicleRegistrationCertificate -> RC.VehicleRegistrationCertificate -> m ()
adjustVehicleOnboardingCounters transporterConfig oldRc newRc = do
  let useUnifiedOnboardingFlagsRecompute = transporterConfig.unifiedOnboardingFlagsRecompute == Just True
  when (useUnifiedOnboardingFlagsRecompute && oldRc.isNew /= Just False) $ RCQuery.updateIsNew (Just False) oldRc.id
  whenJust oldRc.merchantOperatingCityId $ \rcMerchantOpCityId ->
    adjustOnboardingCounters
      useUnifiedOnboardingFlagsRecompute
      CounterVehicle
      rcMerchantOpCityId
      oldRc.fleetOwnerId
      (asAlreadyCounted oldRc.isNew $ bucketsOfFlags (fromMaybe False oldRc.verified) oldRc.approved False)
      (bucketsOfFlags (fromMaybe False newRc.verified) newRc.approved False)

-- | 'adjustDriverOnboardingCounters' for a fleet owner, mirroring 'recomputeFleetFlagsArm'.
adjustFleetOnboardingCounters :: OnboardingFlow m r => DTC.TransporterConfig -> DP.Person -> DFOI.FleetOwnerInformation -> DFOI.FleetOwnerInformation -> m ()
adjustFleetOnboardingCounters transporterConfig person oldInfo newInfo = do
  let useUnifiedOnboardingFlagsRecompute = transporterConfig.unifiedOnboardingFlagsRecompute == Just True
      bucketsOf info = bucketsOfFlags' info.verified info.approved info.enabled info.blocked (isJust info.disabledReasonFlag)
  when (useUnifiedOnboardingFlagsRecompute && oldInfo.isNew /= Just False) $ QFOI.updateIsNew (Just False) person.id
  adjustOnboardingCounters
    useUnifiedOnboardingFlagsRecompute
    CounterFleetOwner
    person.merchantOperatingCityId
    (Just person.id.getId)
    (asAlreadyCounted oldInfo.isNew $ bucketsOf oldInfo)
    (bucketsOf newInfo)

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
markBlockFlags personId change = do
  mbPerson <- QPerson.findById personId
  if maybe False (SDO.isFleetRole . (.role)) mbPerson
    then case change of
      Block p -> QFOIExtra.updateFleetOwnerBlockedStatus True (Just p.bpFlag) personId
      SimpleBlock _ -> QFOIExtra.updateFleetOwnerBlockedStatus True Nothing personId
      Unblock _ -> QFOIExtra.updateFleetOwnerBlockedStatus False Nothing personId
    else case change of
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
markEnableDisableReasonFlags :: OnboardingFlow m r => Bool -> DP.Person -> DisabledChange -> m ()
markEnableDisableReasonFlags unified person change = case change of
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
      else do
        when unified $ DIQueryExtra.updateEnabledReasonFlag (Just DI.AdminEnabled) (cast person.id)
        DIQuery.updateDisabledReasonFlag Nothing (cast person.id)
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
  total <- readBucket "total"
  verified <- readBucket "verified"
  approved <- readBucket "approved"
  rejected <- readBucket "rejected"
  enabled <- readBucket "enabled"
  blocked <- readBucket "blocked"
  disabled <- readBucket "disabled"
  pure $
    OnboardingCounts
      { ocTotal = total,
        ocVerified = verified,
        ocApproved = approved,
        ocRejected = rejected,
        ocEnabled = enabled,
        ocBlocked = blocked,
        ocDisabled = disabled
      }
  where
    keyFor = mkOnboardingCounterKey entity merchantOpCityId mbFleetOwnerId
    readBucket bucket = fromMaybe 0 <$> Hedis.get @Int (keyFor bucket)
