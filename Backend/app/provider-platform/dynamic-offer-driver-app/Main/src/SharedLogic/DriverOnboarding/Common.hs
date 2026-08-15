-- | Shared onboarding primitives: document-validity predicates, fleet/driver association lookups
--   and the enablement side effects. Used by both the flag recompute and the status renderer.
module SharedLogic.DriverOnboarding.Common
  ( overrideInspectionHubAsValid,

    -- * Document validity, shared with the status renderer
    MandatoryMode (..),
    isDocRequiredFor,
    docAppliesToDriver,
    findFleetConfigForRole,
    checkIfDocumentValid,
    checkIfDocumentValid',
    checkAllDocsValid,
    vehicleDocCategory,
    checkAllDriverDocsValid',
    checkAllDriverDocsValidForVerified,
    checkAllDriverDocsValidForEnabling,
    checkAllVehicleDocsValid',
    checkAllVehicleDocsValidForVerified,
    computeApprovedFromDocs,
    docSupportsApproval,
    partitionDocsBySide,

    -- * Fleet / driver association lookups
    hasActiveFleetAssociation,
    isFleetOfDriverDisabled,

    -- * Enablement side effects
    enableDriver,
    sendEnablementSms,
  )
where

import Control.Applicative ((<|>))
import Data.List (partition)
import qualified Data.List as List
import qualified Domain.Types.DocumentOnboardingStage as DOS
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.Person as QPerson
import qualified Tools.SMS as Sms

-- | Treat an InspectionHub document as VALID. Applied by the caller to the document array before
--   handing it to the recompute, so each handler decides whether the inspection gate is waived —
--   mirroring how forceBotApprovalDocValid is applied.
overrideInspectionHubAsValid :: DocumentStatusItem -> DocumentStatusItem
overrideInspectionHubAsValid d
  | d.documentType == DVC.InspectionHub = d {verificationStatus = VALID}
  | otherwise = d

-- | Which "mandatory" flag a doc-validity / doc-type computation should use.
--   The two are deliberately split (see onboarding revamp):
--     * 'ForVerified'  uses @isMandatory@                              — drives the @verified@ flag.
--                      Excludes enabling-only docs (e.g. MSDS, Medical) which are @isMandatory=false@.
--     * 'ForEnabling'  uses @fromMaybe isMandatory isMandatoryForEnabling@ (legacy behaviour) — drives the @enabled@ flag.
data MandatoryMode = ForVerified | ForEnabling
  deriving (Show, Eq)

-- | Resolve whether a doc config counts as mandatory under the given mode.
isDocRequiredFor :: MandatoryMode -> DVC.DocumentVerificationConfig -> Bool
isDocRequiredFor ForVerified config = config.isMandatory
isDocRequiredFor ForEnabling config = fromMaybe config.isMandatory config.isMandatoryForEnabling

-- | Whether a doc applies to this driver, by its `applicableTo` and fleet-linkage. `Nothing` (legacy)
--   = always applies. A non-applicable doc is treated as non-mandatory (doesn't block verified/enabled).
docAppliesToDriver :: Maybe Bool -> DVC.DocumentApplicableType -> Bool
docAppliesToDriver Nothing _ = True
docAppliesToDriver (Just isFleetDriver) applicableTo = case applicableTo of
  DVC.FLEET_AND_INDIVIDUAL -> True
  DVC.FLEET -> isFleetDriver
  DVC.INDIVIDUAL -> not isFleetDriver

-- | Pick the fleet config row for @docType@. Only for a fleet role (FLEET_OWNER/FLEET_BUSINESS) is the
--   role filter applied — prefer the row matching that exact role, falling back to the first @docType@ row.
--   For any other role, behave like the old code: just take the first @docType@ row (role-blind).
findFleetConfigForRole :: DVC.DocumentType -> DP.Role -> [FODVC.FleetOwnerDocumentVerificationConfig] -> Maybe FODVC.FleetOwnerDocumentVerificationConfig
findFleetConfigForRole docType role fleetConfigs
  | SDO.isFleetRole role =
    find (\c -> c.documentType == docType && c.role == role) fleetConfigs
      <|> find (\c -> c.documentType == docType) fleetConfigs
  | otherwise = find (\c -> c.documentType == docType) fleetConfigs

-- | Legacy doc-validity check — uses isMandatoryForEnabling (falling back to isMandatory).
--   This is the entry point for all enable / RC-activation decisions; behaviour is unchanged.
checkIfDocumentValid ::
  DocVerificationConfigs ->
  DP.Role ->
  DDVC.DocumentType ->
  DVC.VehicleCategory ->
  ResponseStatus ->
  Maybe Bool ->
  Bool
checkIfDocumentValid = checkIfDocumentValid' ForEnabling Nothing

checkIfDocumentValid' ::
  MandatoryMode ->
  Maybe Bool ->
  DocVerificationConfigs ->
  DP.Role ->
  DDVC.DocumentType ->
  DVC.VehicleCategory ->
  ResponseStatus ->
  Maybe Bool ->
  Bool
checkIfDocumentValid' _ _ _ _ _ _ VALID _ = True
checkIfDocumentValid' mode _mbIsFleetDriver (Left fleetConfigs) role docType _category status _makeSelfieAadhaarPanMandatory = do
  let mbConfig = find (\config -> config.documentType == docType && config.role == role) fleetConfigs
      -- ForVerified uses isMandatory; ForEnabling uses isMandatoryForEnabling (falling back to isMandatory).
      isMandatoryForFleet config = case mode of
        ForVerified -> config.isMandatory
        ForEnabling -> fromMaybe config.isMandatory config.isMandatoryForEnabling
  case mbConfig of
    Just config ->
      not (isMandatoryForFleet config)
        || ( case status of
               MANUAL_VERIFICATION_REQUIRED -> case mode of
                 ForVerified -> fromMaybe True config.isDefaultVerifiedOnManualVerification
                 ForEnabling -> config.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True
checkIfDocumentValid' mode mbIsFleetDriver (Right driverConfigs) _role docType category status makeSelfieAadhaarPanMandatory = do
  let mbVerificationConfig = find (\config -> config.documentType == docType && config.vehicleCategory == category) driverConfigs
  case mbVerificationConfig of
    Just verificationConfig ->
      not (isDocRequiredFor mode verificationConfig && docAppliesToDriver mbIsFleetDriver verificationConfig.applicableTo && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory))
        || ( case status of
               MANUAL_VERIFICATION_REQUIRED -> case mode of
                 ForVerified -> fromMaybe True verificationConfig.isDefaultVerifiedOnManualVerification
                 ForEnabling -> verificationConfig.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True

documentOnboardingStageTag :: DOS.DocumentOnboardingStage -> Text
documentOnboardingStageTag = \case
  DOS.PersonalOnboarding -> "PersonalOnboarding"
  DOS.VehicleDetailsStage -> "VehicleDetailsStage"
  DOS.CompanyOnboarding -> "CompanyOnboarding"
  DOS.TaxAndLegal _ -> "TaxAndLegal"
  DOS.BankDetails -> "BankDetails"

documentOnboardingStageOf :: DocVerificationConfigs -> DP.Role -> Maybe DVC.VehicleCategory -> DDVC.DocumentType -> Maybe DOS.DocumentOnboardingStage
documentOnboardingStageOf (Left fleetConfigs) role _mbCategory docType =
  (.documentOnboardingStage) =<< findFleetConfigForRole docType role fleetConfigs
documentOnboardingStageOf (Right driverConfigs) _role mbCategory docType =
  (.documentOnboardingStage)
    =<< ( find (\c -> c.documentType == docType && maybe False (== c.vehicleCategory) mbCategory) driverConfigs
            <|> find (\c -> c.documentType == docType) driverConfigs
        )

checkAllDocsByStage ::
  (DDVC.DocumentType -> Maybe DOS.DocumentOnboardingStage) ->
  (DocumentStatusItem -> Maybe Bool) ->
  [DocumentStatusItem] ->
  Maybe Bool
checkAllDocsByStage stageOf checkDoc docs =
  allValid $ map anyStageValid $ groupOn (fmap documentOnboardingStageTag . stageOf) docs
  where
    anyStageValid tagDocs = anyValid $ map (allValid . map checkDoc) $ groupOn stageOf tagDocs
    allValid results
      | Just False `elem` results = Just False
      | Nothing `elem` results = Nothing
      | otherwise = Just True
    anyValid results
      | Just True `elem` results = Just True
      | Nothing `elem` results = Nothing
      | otherwise = Just False
    groupOn :: Ord b => (DDVC.DocumentType -> b) -> [DocumentStatusItem] -> [[DocumentStatusItem]]
    groupOn f = List.groupBy (\x y -> f x.documentType == f y.documentType) . List.sortOn (f . (.documentType))

checkAllDocsValid ::
  MandatoryMode ->
  Maybe Bool ->
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDocsValid mode mbIsFleetDriver configs role docs category makeSelfieAadhaarPanMandatory = do
  let isValid doc = checkIfDocumentValid' mode mbIsFleetDriver configs role doc.documentType category doc.verificationStatus makeSelfieAadhaarPanMandatory
  checkAllDocsByStage (documentOnboardingStageOf configs role (Just category)) (Just . isValid) docs == Just True

checkAllDriverDocsValid' ::
  MandatoryMode ->
  Maybe Bool ->
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsValid' = checkAllDocsValid

-- | Are all isMandatory driver docs VALID? Excludes isMandatoryForEnabling-only docs. Drives
--   @verified@. (Sibling checkAllDriverDocsValidForEnabling uses the isMandatoryForEnabling set → @enabled@.)
checkAllDriverDocsValidForVerified ::
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsValidForVerified = checkAllDriverDocsValid' ForVerified Nothing

-- | All driver docs in the enabling set (isMandatoryForEnabling) VALID; no applicableTo filtering. Drives @enabled@.
checkAllDriverDocsValidForEnabling ::
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsValidForEnabling = checkAllDriverDocsValid' ForEnabling Nothing

checkAllVehicleDocsValid' ::
  MandatoryMode ->
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValid' mode driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory =
  -- Vehicle docs are not subject to the fleet-driver/DCO applicableTo split → Nothing.
  checkAllDocsValid mode Nothing (Right driverDocConfs) DP.DRIVER vehicleDoc.documents (vehicleDocCategory vehicleDoc) makeSelfieAadhaarPanMandatory

vehicleDocCategory :: VehicleDocumentItem -> DVC.VehicleCategory
vehicleDocCategory vehicleDoc = fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory

-- | Are all isMandatory vehicle docs VALID? Excludes isMandatoryForEnabling-only docs. Drives
--   @verified@. (Sibling checkAllVehicleDocsValidForEnabling uses the isMandatoryForEnabling set → @enabled@.)
checkAllVehicleDocsValidForVerified ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValidForVerified = checkAllVehicleDocsValid' ForVerified

computeApprovedFromDocs :: Maybe Bool -> DocVerificationConfigs -> DP.Role -> [DocumentStatusItem] -> Maybe Bool
computeApprovedFromDocs mbIsFleetDriver configs role docs =
  let approvalDocs = filter (docSupportsApproval mbIsFleetDriver configs role) docs
   in checkAllDocsByStage (documentOnboardingStageOf configs role Nothing) computeApprovedDoc' approvalDocs
  where
    computeApprovedDoc' d = case d.verificationStatus of
      VALID -> Just True
      INVALID -> Just False
      FAILED -> Just False
      _ -> Nothing

docSupportsApproval :: Maybe Bool -> DocVerificationConfigs -> DP.Role -> DocumentStatusItem -> Bool
docSupportsApproval _mbIsFleetDriver (Left fleetConfigs) role d =
  case findFleetConfigForRole d.documentType role fleetConfigs of
    Just c -> c.isApprovalSupported == Just True
    Nothing -> False
docSupportsApproval mbIsFleetDriver (Right driverConfigs) _role d =
  case find (\c -> c.documentType == d.documentType) driverConfigs of
    Just c -> c.isApprovalSupported == Just True && docAppliesToDriver mbIsFleetDriver c.applicableTo
    Nothing -> False

-- | Split a person's documents into (driver-side, vehicle-side). Fleet-owner configs carry no
--   vehicle split, so every fleet document stays on the person side.
partitionDocsBySide :: DocVerificationConfigs -> [DocumentStatusItem] -> ([DocumentStatusItem], [DocumentStatusItem])
partitionDocsBySide (Left _) docs = (docs, [])
partitionDocsBySide (Right driverConfigs) docs = partition isDriverSide docs
  where
    categoryOf docType = (.documentCategory) =<< find (\c -> c.documentType == docType) driverConfigs
    isDriverSide d = isDriverSideDocType (categoryOf d.documentType) d.documentType

hasActiveFleetAssociation :: OnboardingFlow m r => Id DP.Person -> m Bool
hasActiveFleetAssociation personId = isJust <$> QFDA.findByDriverId personId True

-- | Is the fleet this driver is actively associated with currently flagged disabled? Used to pull
--   the fleet's disablement down onto the driver at recompute time, instead of pushing it out to
--   every driver when the fleet flips.
isFleetOfDriverDisabled :: OnboardingFlow m r => Id DP.Person -> m Bool
isFleetOfDriverDisabled driverId = do
  mbFda <- QFDA.findByDriverId driverId True
  case mbFda of
    Nothing -> pure False
    Just fda -> do
      mbFleetOwnerInfo <- QFOI.findByPrimaryKey (Id fda.fleetOwnerId)
      pure $ maybe False (isJust . (.disabledReasonFlag)) mbFleetOwnerInfo

-- | Enable a driver/fleet (cascades fleet→drivers). @verifiedToSet@ is written for `verified` too;
--   legacy callers pass True.
enableDriver :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> Id DP.Person -> DP.Role -> Maybe Text -> DTC.TransporterConfig -> Id DM.Merchant -> Bool -> m ()
enableDriver merchantOpCityId personId role driverName transporterConfig merchantId verifiedToSet = do
  if SDO.isFleetRole role
    then do
      fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      unless (fleetOwnerInfo.enabled && fleetOwnerInfo.verified) $ do
        QFOI.updateFleetOwnerEnabledAndVerifiedStatus True verifiedToSet personId
        -- Clearing the fleet's flag is enough; drivers drop their FleetDisabled stamp on their
        -- own recompute.
        QFOI.updateFleetOwnerDisabledReasonFlag Nothing personId
    else do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
      unless (driverInfo.enabled && driverInfo.verified) $ do
        SDO.enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId verifiedToSet
        whenJust driverName $ \name -> QPerson.updateName name personId
        sendEnablementSms merchantOpCityId personId transporterConfig merchantId

sendEnablementSms :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> Id DP.Person -> DTC.TransporterConfig -> Id DM.Merchant -> m ()
sendEnablementSms merchantOpCityId personId transporterConfig merchantId =
  when (transporterConfig.sendSmsOnEnablement == Just True) $ do
    fork "sending sms - onboarding" $ do
      driver <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      smsCfg <- asks (.smsCfg)
      mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
      let countryCode = fromMaybe "+91" driver.mobileCountryCode
          phoneNumber = countryCode <> mobileNumber
          sender = smsCfg.sender
      (mbSender, message, templateId, messageType) <-
        MessageBuilder.buildOnboardingMessage merchantOpCityId $
          MessageBuilder.BuildOnboardingMessageReq {}
      Sms.sendSMS merchantId merchantOpCityId (Sms.SendSMSReq message phoneNumber (fromMaybe sender mbSender) templateId messageType) >>= Sms.checkSmsResult
