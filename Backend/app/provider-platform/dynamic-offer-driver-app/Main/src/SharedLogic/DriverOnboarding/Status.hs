module SharedLogic.DriverOnboarding.Status
  ( ResponseStatus (..),
    StatusRes' (..),
    VehicleDocumentItem (..),
    DocumentStatusItem (..),
    CommonDocumentItem (..),
    DLDetails (..),
    RCDetails (..),
    statusHandler',
    getDLAndStatus,
    getRCAndStatus,
    getAadhaarStatus,
    mapStatus,
    fetchAndCheckVehicleDocsValidForEnabling,
    checkAllVehicleDocsValidForFetchedDocs,
    fetchAndCheckDriverDocsValidForEnabling,
    checkAllDriverDocsValidForFetchedDocs,
    validateMandatoryVehicleDocsForRC,
    fetchVehicleDocStatusesForRC,
    fetchDriverDocStatusesForPerson,
    invalidDependencyDocs,
    getFleetDocVerificationConfig,
    findFleetDocVerificationConfig,
    hasActiveFleetAssociation,
    botApproveAndReconcile,
    recomputeOnboardingFlags,
    activateRCAutomatically,
    mkCommonDocumentItem,
    mkDLMetadata,
    mkAadhaarMetadata,
    mkPanMetadata,
    mkGSTMetadata,
    mkUDYAMMetadata,
    checkInspectionHubRequestCreated,
    getInspectionHubStatusAndReason,
    checkLMSTrainingStatus,
    runRefreshOnboardingFlagsDriver,
    runRefreshOnboardingFlagsFleet,
    runRefreshOnboardingFlagsVehicle,
    runRefreshOnboardingFlagsVehicleWithBotApproval,
    ensureNoActiveRidesUnderFleet,
  )
where

import qualified API.Types.UI.DriverOnboardingV2 as DOVT
import Control.Applicative ((<|>))
import Control.Monad.Extra (anyM)
import Data.Either (fromRight)
import Data.List (nub, sortOn)
import Data.Ord (Down (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import Domain.Types.CommonDocumentData (renderCommonDocumentData)
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCDOD
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverGstin as DGstin
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLicense as DL
import qualified Domain.Types.DriverPanCard as DPan
import qualified Domain.Types.DriverUdyam as DUdyam
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperationHubRequests as DOHR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import GHC.Records.Extra (HasField)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types (Language, ServiceFlow)
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude hiding (HasField)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (HasField)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified SharedLogic.DriverIdentityInfo as DIInfo
import qualified SharedLogic.DriverOnboarding as SDO
import SharedLogic.DriverOnboarding.Common
import qualified SharedLogic.DriverOnboarding.Digilocker as SDDigilocker
import SharedLogic.DriverOnboarding.OnboardingFlags.Flow
import SharedLogic.DriverOnboarding.OnboardingFlags.Types (OnboardingFlow)
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified SharedLogic.PersonBankAccount as SPBA
import qualified Storage.Beam.IssueManagement ()
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.FleetOwnerDocumentVerificationConfig (FleetOwnerDocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as BVQuery
import qualified Storage.Queries.CommonDriverOnboardingDocumentsExtra as QCommonDocExtra
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverBankAccount as QDriverBankAccount
import qualified Storage.Queries.DriverGstin as QDGST
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverOperatorAssociation as QDOA
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverSSN as QDSSN
import qualified Storage.Queries.DriverUdyam as QUDYAM
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOperatorAssociationExtra as QFOA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RideExtra as QRideExtra
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.BackgroundVerification as BackgroundVerification
import Tools.Error (DocumentVerificationConfigError (..))
import qualified Tools.Plasma as TPlasma
import qualified Tools.Verification as Verification

data PersonStatusContext = PersonStatusContext
  { statusPerson :: DP.Person,
    statusEntityImagesInfo :: IQuery.EntityImagesInfo
  }

data VehicleDocsContext = VehicleDocsContext
  { allDocVerificationConfigs :: DocVerificationConfigs,
    driverDocConfigs :: [DVC.DocumentVerificationConfig],
    vehicleDocumentsUnverified :: [VehicleDocumentItem]
  }

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- UNAUTHORIZED is used when a driver is not eligible to be onboarded to the platform
-- INVALID is the state
--   which the doc switches to when, for example, it's expired or when it is invalidated from dashboard.
-- PULL_REQUIRED is used when a document needs to be pulled from DigiLocker
-- CONSENT_DENIED is used when user denies consent for DigiLocker verification

-- | The applicable dependency docs that are NOT VALID (empty ⇒ all good). Counts only deps that apply per
--   dvc `applicableTo`: @mbIsFleetDriver = Nothing@ disables the split (vehicle / fleet-owner / legacy → all
--   deps required); @driverConfigs@ supplies each dep's `applicableTo` (pass [] when no split is needed).
--   Returns the offending doc types so callers can report them.
invalidDependencyDocs :: Maybe Bool -> [DVC.DocumentVerificationConfig] -> [DVC.DocumentType] -> [DocumentStatusItem] -> [DVC.DocumentType]
invalidDependencyDocs mbIsFleetDriver driverConfigs deps docStatuses =
  filter (\dep -> dep `notElem` validDocTypes) (filter depApplies deps)
  where
    validDocTypes = map (.documentType) $ filter (\d -> d.verificationStatus == VALID) docStatuses
    depApplies dep = maybe True (\c -> docAppliesToDriver mbIsFleetDriver c.applicableTo) (find (\c -> c.documentType == dep) driverConfigs)

-- | Non-throwing: fetch fleet configs for @docType@ via config-pilot (in-mem cached) and pick the row for
--   @role@ (see 'findFleetConfigForRole'). Returns Nothing when the city has no such config.
findFleetDocVerificationConfig :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DVC.DocumentType -> DP.Role -> m (Maybe FODVC.FleetOwnerDocumentVerificationConfig)
findFleetDocVerificationConfig merchantOpCityId docType role = do
  configs <-
    getConfig
      (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just docType, role = Just role})
      (Just (filter (\c -> c.documentType == docType) <$> CQFODVC.findAllByMerchantOpCityId merchantOpCityId Nothing))
  pure $ findFleetConfigForRole docType role configs

-- | Throwing variant of 'findFleetDocVerificationConfig' — errors if the city has no such config.
getFleetDocVerificationConfig :: OnboardingFlow m r => Id DMOC.MerchantOperatingCity -> DVC.DocumentType -> DP.Role -> m FODVC.FleetOwnerDocumentVerificationConfig
getFleetDocVerificationConfig merchantOpCityId docType role =
  findFleetDocVerificationConfig merchantOpCityId docType role
    >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show docType))

data StatusRes' = StatusRes'
  { driverDocuments :: [DocumentStatusItem],
    vehicleDocuments :: [VehicleDocumentItem],
    enabled :: Bool,
    verified :: Bool,
    approved :: Maybe Bool,
    blocked :: Bool,
    blockedReason :: Maybe Text,
    onboardingAs :: Maybe DI.OnboardingAs,
    disabledReasonFlag :: Maybe DI.DisabledReasonFlag,
    recentFleetInfo :: Maybe DOVT.FleetInfo,
    manualVerificationRequired :: Maybe Bool,
    driverLicenseDetails :: Maybe [DLDetails],
    vehicleRegistrationCertificateDetails :: Maybe [RCDetails],
    digilockerResponseCode :: Maybe Text,
    digilockerAuthorizationUrl :: Maybe Text
  }

data CommonDocumentItem = CommonDocumentItem
  { documentType :: DDVC.DocumentType,
    documentData :: Text,
    verificationStatus :: ResponseStatus,
    rejectReason :: Maybe Text,
    documentImageId :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data DLDetails = DLDetails
  { driverName :: Maybe Text,
    driverLicenseNumber :: Text,
    operatingCity :: Text,
    driverDateOfBirth :: Maybe UTCTime,
    classOfVehicles :: [Text],
    imageId1 :: Text,
    imageId2 :: Maybe Text,
    dateOfIssue :: Maybe UTCTime,
    createdAt :: UTCTime,
    s3Path1 :: Maybe Text,
    s3Path2 :: Maybe Text,
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data RCDetails = RCDetails
  { vehicleRegistrationCertNumber :: Text,
    imageId :: Text,
    operatingCity :: Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleCategory :: Maybe Text,
    airConditioned :: Maybe Bool,
    vehicleManufacturer :: Maybe Text,
    vehicleModel :: Maybe Text,
    vehicleColor :: Maybe Text,
    vehicleDoors :: Maybe Int,
    vehicleSeatBelts :: Maybe Int,
    vehicleModelYear :: Maybe Int,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    createdAt :: UTCTime,
    failedRules :: [Text],
    verificationStatus :: Maybe Documents.VerificationStatus,
    s3Path :: Maybe Text,
    documentExpiry :: Maybe UTCTime,
    permitExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- | Fetch a specific RC's docs with their per-doc statuses. @onlyMandatoryDocs = Just True@ fetches
--   the mandatory set; @Nothing@ fetches the full default list (needed e.g. for the dependency check,
--   whose deps may not be mandatory). Extracted from fetchAndCheckVehicleDocsValidForEnabling so callers that
--   need the per-doc statuses (not just the Bool) can reuse the same fetch.
fetchVehicleDocStatusesForRC ::
  OnboardingFlow m r =>
  RC.VehicleRegistrationCertificate ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Maybe Bool ->
  Bool ->
  Bool ->
  m (VehicleDocumentItem, [DVC.DocumentVerificationConfig])
fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig language reqRegistrationNo onlyMandatoryDocs enableDocumentMetadata skipMessages = do
  let entity = IQuery.VehicleRCEntity rc
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now, enableDocumentMetadata}
  allDocumentVerificationConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing))
  vehicleDocumentsUnverified <- fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs skipMessages
  vehicleDoc <-
    find (\doc -> doc.registrationNo == reqRegistrationNo) vehicleDocumentsUnverified
      & fromMaybeM (InvalidRequest $ "Vehicle doc not found for vehicle with registartionNo " <> reqRegistrationNo)
  -- Surface the RC validation failures (invalid fuel type / vehicle class / OEM / manufacturing year, as
  -- computed by validateRCResponse and persisted in RC.failedRules) in the RC document's verificationMessage,
  -- mirroring the /register/status behaviour (getRCAndStatus). Only messageful callers (skipMessages == False)
  -- get this; the validity-only caller keeps skipping translations.
  vehicleDocWithReasons <- appendRcFailedRulesToVehicleDoc language rc vehicleDoc
  pure (vehicleDocWithReasons, allDocumentVerificationConfigs)

-- | Append this RC's @failedRules@ to the RC document's verificationMessage (only when it already has a base
--   message and is not VALID); every other document is left untouched.
appendRcFailedRulesToVehicleDoc :: OnboardingFlow m r => Language -> RC.VehicleRegistrationCertificate -> VehicleDocumentItem -> m VehicleDocumentItem
appendRcFailedRulesToVehicleDoc language rc vehicleDoc
  | null rc.failedRules = pure vehicleDoc
  | otherwise = do
    documents <- forM vehicleDoc.documents $ \doc ->
      case (doc.documentType, doc.verificationStatus, doc.verificationMessage) of
        (DVC.VehicleRegistrationCertificate, status, Just msg)
          | status /= VALID -> do
            msgWithReasons <- addVerificationReasons language (Just rc.failedRules) msg
            pure doc {verificationMessage = Just msgWithReasons}
        _ -> pure doc
    pure vehicleDoc {documents = documents}

-- | Fetch this RC's vehicle docs and check all enabling docs are VALID (non-BOT path; matches main's ForEnabling).
fetchAndCheckVehicleDocsValidForEnabling ::
  OnboardingFlow m r =>
  RC.VehicleRegistrationCertificate ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  m Bool
fetchAndCheckVehicleDocsValidForEnabling rc merchantOperatingCity transporterConfig language reqRegistrationNo = do
  (vehicleDoc, allDocumentVerificationConfigs) <- fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig language reqRegistrationNo (Just True) False True
  pure $ checkAllVehicleDocsValidForEnabling allDocumentVerificationConfigs vehicleDoc Nothing

-- | All mandatory vehicle docs VALID, over already-fetched statuses (no fetch).
checkAllVehicleDocsValidForFetchedDocs ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Bool
checkAllVehicleDocsValidForFetchedDocs allDocumentVerificationConfigs vehicleDoc =
  checkAllVehicleDocsValidForVerified allDocumentVerificationConfigs vehicleDoc Nothing

-- | Fetch this driver's docs and check all enabling docs are VALID (non-BOT path; matches main's ForEnabling).
fetchAndCheckDriverDocsValidForEnabling ::
  OnboardingFlow m r =>
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  m Bool
fetchAndCheckDriverDocsValidForEnabling person merchantOperatingCity transporterConfig language = do
  (allDocVerificationConfigs, driverDocuments, vehicleCategory, _vehicleDocuments) <- fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language (Just True)
  pure $ checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory Nothing

-- | All mandatory driver docs VALID, over already-fetched statuses (no fetch); applies the fleet filter.
checkAllDriverDocsValidForFetchedDocs ::
  OnboardingFlow m r =>
  DP.Person ->
  DTC.TransporterConfig ->
  DocVerificationConfigs ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  m Bool
checkAllDriverDocsValidForFetchedDocs person transporterConfig allDocVerificationConfigs driverDocuments vehicleCategory = do
  mbIsFleetDriver <-
    if transporterConfig.enableBotFlow == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True
      then Just . isJust <$> QFDA.findByDriverId person.id True
      else pure Nothing
  pure $ checkAllDriverDocsValid' ForVerified mbIsFleetDriver allDocVerificationConfigs person.role driverDocuments vehicleCategory Nothing

-- | Fetch a person's driver docs with their per-doc statuses, plus the configs and the active vehicle
--   category. @onlyMandatoryDocs = Just True@ fetches the mandatory set; @Nothing@ fetches the full
--   default list (needed e.g. for the dependency check). Extracted from fetchAndCheckDriverDocsValidForEnabling
--   so callers that need the per-doc statuses (not just the Bool) can reuse the same fetch.
fetchDriverDocStatusesForPerson ::
  OnboardingFlow m r =>
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Maybe Bool ->
  m (DocVerificationConfigs, [DocumentStatusItem], DVC.VehicleCategory, [VehicleDocumentItem])
fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language onlyMandatoryDocs = do
  let useHVSdkForDL = Just True
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now, enableDocumentMetadata = False}
  let skipMessages = True -- Skip translations, only need status check for inspection
  VehicleDocsContext {allDocVerificationConfigs, vehicleDocumentsUnverified} <-
    buildVehicleDocsContext person entityImagesInfo language onlyMandatoryDocs skipMessages Nothing
  let possibleVehicleCategoriesRaw = nub $ do
        vehicleDocumentsUnverified <&> \vehicleDoc -> do
          fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      possibleVehicleCategories = if null possibleVehicleCategoriesRaw then [DVC.CAR] else possibleVehicleCategoriesRaw
  driverDocuments <- fetchDriverDocuments entityImagesInfo allDocVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs skipMessages
  let vehicleCategory = case vehicleDocumentsUnverified of
        (doc : _) -> fromMaybe doc.userSelectedVehicleCategory doc.verifiedVehicleCategory
        [] -> DVC.CAR
  pure (allDocVerificationConfigs, driverDocuments, vehicleCategory, vehicleDocumentsUnverified)

onboardingLockTTLSeconds :: Int
onboardingLockTTLSeconds = 15

mkPersonDocsStatusKey :: Id DP.Person -> Text
mkPersonDocsStatusKey personId = "DocsStatus:Person:" <> personId.getId

mkRCDocsStatusKey :: Id RC.VehicleRegistrationCertificate -> Text
mkRCDocsStatusKey rcId = "DocsStatus:RC:" <> rcId.getId

-- | Refresh a person's onboarding flags. Under `unifiedOnboardingFlagsRecompute` this reads the
--   person's documents and recomputes the flags directly through the common entry point, without
--   running the status *renderer*. Otherwise it keeps the historical behaviour of driving the
--   side effects out of statusHandler'.
--
--   Note the two paths differ in what they return: the legacy path reports the flag as persisted
--   (read back after the writes), the unified path reports the value it just computed.
runRefreshOnboardingFlagsDriver :: OnboardingFlow m r => Maybe DP.Person -> Maybe DTC.TransporterConfig -> Id DP.Person -> m (Maybe Bool)
runRefreshOnboardingFlagsDriver mbPerson mbTransporterConfig personId =
  Hedis.withLockRedisAndReturnValue (mkPersonDocsStatusKey personId) onboardingLockTTLSeconds $ do
    PersonStatusContext {statusPerson, statusEntityImagesInfo} <- loadPersonStatusContext mbPerson mbTransporterConfig personId
    let transporterConfig = statusEntityImagesInfo.transporterConfig
        merchantOperatingCity = statusEntityImagesInfo.merchantOperatingCity
    if transporterConfig.unifiedOnboardingFlagsRecompute == Just True
      then do
        let language = fromMaybe merchantOperatingCity.language statusPerson.language
        (allDocVerificationConfigs, driverDocuments, vehicleCategory, vehicleDocuments) <-
          fetchDriverDocStatusesForPerson statusPerson merchantOperatingCity transporterConfig language (Just True)
        res <-
          recomputeOnboardingFlags
            OnboardingFlagsInput
              { ofiPerson =
                  Just
                    PersonFlagsCtx
                      { pfcPerson = statusPerson,
                        pfcMerchantOpCityId = merchantOperatingCity.id,
                        pfcMerchantId = merchantOperatingCity.merchantId,
                        pfcTransporterConfig = transporterConfig,
                        pfcConfigs = allDocVerificationConfigs,
                        pfcDocs = driverDocuments,
                        pfcVehicleCategory = vehicleCategory,
                        pfcMakeSelfieAadhaarPanMandatory = Nothing,
                        pfcDriverName = Nothing,
                        pfcOnboardingVehicleCategory = Nothing,
                        pfcIsFleetDriver = Nothing,
                        pfcVehicleDocs = vehicleDocuments
                      },
                ofiVehicles = []
              }
            True
        pure res.ofrPersonEnabled
      else do
        statusRes <- statusHandler' statusPerson statusEntityImagesInfo Nothing Nothing Nothing Nothing (Just True) False (Just True) True Nothing
        pure $ Just statusRes.enabled

runRefreshOnboardingFlagsFleet :: OnboardingFlow m r => Maybe DP.Person -> Maybe DTC.TransporterConfig -> Id DP.Person -> m (Maybe Bool)
runRefreshOnboardingFlagsFleet = runRefreshOnboardingFlagsDriver

runRefreshOnboardingFlagsVehicle :: OnboardingFlow m r => Maybe DTC.TransporterConfig -> Id RC.VehicleRegistrationCertificate -> m (Maybe Bool)
runRefreshOnboardingFlagsVehicle mbTransporterConfig = runRefreshOnboardingFlagsVehicleWithBotApproval mbTransporterConfig False

runRefreshOnboardingFlagsVehicleWithBotApproval :: OnboardingFlow m r => Maybe DTC.TransporterConfig -> Bool -> Id RC.VehicleRegistrationCertificate -> m (Maybe Bool)
runRefreshOnboardingFlagsVehicleWithBotApproval mbTransporterConfig forceBotApproval rcId = do
  Hedis.withLockRedisAndReturnValue (mkRCDocsStatusKey rcId) onboardingLockTTLSeconds $ do
    rc <- RCQuery.findById rcId >>= fromMaybeM (InternalError $ "RC not found by id: " <> rcId.getId)
    merchantOpCityId <- rc.merchantOperatingCityId & fromMaybeM (InternalError $ "merchantOperatingCityId missing for RC " <> rc.id.getId)
    merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
    transporterConfig <-
      maybe
        (getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId))
        pure
        mbTransporterConfig
    let useUnifiedOnboardingFlagsRecompute = transporterConfig.unifiedOnboardingFlagsRecompute == Just True
    registrationNo <- decrypt rc.certificateNumber
    (vehicleDocItem, allDocumentVerificationConfigs) <- fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig merchantOperatingCity.language registrationNo (Just True) False True
    -- Waive the InspectionHub gate here (this is the full vehicle recompute), and force
    -- BotApproval VALID when the caller asked for it. Both are document-array transforms so the
    -- recompute itself stays policy-free.
    let withInspectionWaived = vehicleDocItem {documents = map overrideInspectionHubAsValid vehicleDocItem.documents}
        vehicleDocItem' = if forceBotApproval then withInspectionWaived {documents = map forceBotApprovalDocValid withInspectionWaived.documents} else withInspectionWaived
    void $
      recomputeOnboardingFlags
        OnboardingFlagsInput
          { ofiPerson = Nothing,
            ofiVehicles =
              [ VehicleDocsEntry
                  { vdeRegistrationNo = registrationNo,
                    vdeItem = vehicleDocItem',
                    vdeConfigs = allDocumentVerificationConfigs,
                    vdeMakeSelfieAadhaarPanMandatory = Nothing
                  }
              ]
          }
        useUnifiedOnboardingFlagsRecompute
  pure Nothing

forceBotApprovalDocValid :: DocumentStatusItem -> DocumentStatusItem
forceBotApprovalDocValid d
  | d.documentType == DVC.BotApproval = d {verificationStatus = VALID}
  | otherwise = d

loadPersonStatusContext ::
  OnboardingFlow m r =>
  Maybe DP.Person ->
  Maybe DTC.TransporterConfig ->
  Id DP.Person ->
  m PersonStatusContext
loadPersonStatusContext mbPerson mbTransporterConfig personId = do
  person <- maybe (runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)) pure mbPerson
  transporterConfig <-
    maybe
      (getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId))
      pure
      mbTransporterConfig
  merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let statusEntityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now, enableDocumentMetadata = False}
  pure PersonStatusContext {statusPerson = person, statusEntityImagesInfo}

buildVehicleDocsContext ::
  OnboardingFlow m r =>
  DP.Person ->
  IQuery.EntityImagesInfo ->
  Language ->
  Maybe Bool ->
  Bool ->
  Maybe Text ->
  m VehicleDocsContext
buildVehicleDocsContext person entityImagesInfo language onlyMandatoryDocs skipMessages mbReqRegistrationNo = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
  allDocVerificationConfigs <-
    if SDO.isFleetRole person.role
      then Left <$> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Nothing}) Nothing
      else Right <$> getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, vehicleCategory = Nothing}) Nothing
  let baseDriverDocConfigs = fromRight [] allDocVerificationConfigs :: [DVC.DocumentVerificationConfig]
  (driverDocConfigs, vehicleDocumentsUnverified) <-
    case (SDO.isFleetRole person.role, mbReqRegistrationNo) of
      (True, Just reqRegistrationNo) -> do
        (vehicleDocItem, vehicleDocConfigs) <- fetchFleetOwnerVehicleDocs reqRegistrationNo
        pure (vehicleDocConfigs, [vehicleDocItem])
      (True, Nothing) -> pure (baseDriverDocConfigs, [])
      (False, _) -> (baseDriverDocConfigs,) <$> fetchVehicleDocuments entityImagesInfo baseDriverDocConfigs language mbReqRegistrationNo onlyMandatoryDocs skipMessages
  pure VehicleDocsContext {allDocVerificationConfigs, driverDocConfigs, vehicleDocumentsUnverified}
  where
    fetchFleetOwnerVehicleDocs reqRegistrationNo = do
      let merchantOperatingCity = entityImagesInfo.merchantOperatingCity
          transporterConfig = entityImagesInfo.transporterConfig
          registrationNo = normalizeRegistrationNo reqRegistrationNo
      rcNoEnc <- encrypt registrationNo
      rc <-
        RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
          >>= fromMaybeM (InvalidRequest $ "Vehicle not found with registrationNo " <> registrationNo)
      fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig language registrationNo onlyMandatoryDocs entityImagesInfo.enableDocumentMetadata skipMessages

normalizeRegistrationNo :: Text -> Text
normalizeRegistrationNo = T.toUpper . SDO.removeSpaceAndDash

-- | The onboarding status engine. Builds the per-document status list returned to the app, and as a
--   side-effect mutates onboarding state:
--     • enableBotFlow on  — recompute* are the source of truth for verified/enabled (both directions);
--       `approved` is BOT-owned; RC auto-activation and legacy inline auto-enable are suppressed.
--     • enableBotFlow off — legacy: auto-enable/RC-activate/fleet-disable inline (unchanged).
statusHandler' ::
  OnboardingFlow m r =>
  DP.Person ->
  IQuery.EntityImagesInfo ->
  Maybe Bool ->
  Maybe Bool ->
  Maybe DVC.VehicleCategory ->
  Maybe DL.DriverLicense ->
  Maybe Bool ->
  Bool ->
  Maybe Bool ->
  Bool ->
  Maybe Text ->
  m StatusRes'
statusHandler' person entityImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc onlyMandatoryDocs skipMessages mbReqRegistrationNo = do
  let merchantId = entityImagesInfo.merchantOperatingCity.merchantId
      merchantOperatingCity = entityImagesInfo.merchantOperatingCity
      merchantOpCityId = merchantOperatingCity.id
      transporterConfig = entityImagesInfo.transporterConfig
      personId = person.id
  let language = fromMaybe merchantOperatingCity.language person.language

  let mbFetchScopedRegistrationNo = if SDO.isFleetRole person.role then mbReqRegistrationNo else Nothing
  VehicleDocsContext {allDocVerificationConfigs, driverDocConfigs, vehicleDocumentsUnverified} <-
    buildVehicleDocsContext person entityImagesInfo language onlyMandatoryDocs skipMessages mbFetchScopedRegistrationNo

  let vehicleCategoryWithoutMandatoryConfigs = case onboardingVehicleCategory <|> (mDL >>= (.vehicleCategory)) of
        Just vehicleCategory -> do
          let vehicleDocumentVerificationConfigs = filter (\config -> config.vehicleCategory == vehicleCategory) driverDocConfigs
          let mandatoryVehicleDocumentVerificationConfigs = filter (\config -> config.documentType `elem` SDO.defaultVehicleDocumentTypes && fromMaybe config.isMandatory config.isMandatoryForEnabling) vehicleDocumentVerificationConfigs
          if null mandatoryVehicleDocumentVerificationConfigs then Just vehicleCategory else Nothing
        Nothing -> Nothing

  let possibleVehicleCategoriesRaw = nub $
        (maybeToList vehicleCategoryWithoutMandatoryConfigs <>) $ do
          vehicleDocumentsUnverified <&> \vehicleDoc -> do
            fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory
      -- Ensure at least one category is provided to avoid empty list bug
      -- If no vehicle categories found, use CAR as fallback (same as fallback used later in vehicleCategory)
      possibleVehicleCategories = if null possibleVehicleCategoriesRaw then [DVC.CAR] else possibleVehicleCategoriesRaw

  driverDocuments <- fetchDriverDocuments entityImagesInfo allDocVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs skipMessages

  let enableBotFlow = transporterConfig.enableBotFlow == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True

  vehicleDocuments <-
    if enableBotFlow
      then do
        -- BOT flow: verified/enabled are purely doc-driven (see recomputeDriverVerifiedAndEnabled / recomputeFleetVerifiedAndEnabled),
        -- independent of separateDriverVehicleEnablement. The BOT sets `approved`; statusHandler derives the rest.
        let vehicleCategory = fromMaybe DVC.CAR $ onboardingVehicleCategory <|> listToMaybe possibleVehicleCategories
        -- Person flags go through the one common entry point; it picks fleet-owner vs driver from the
        -- config source. Roles that are neither get no write, as before.
        when (SDO.isFleetRole person.role || person.role == DP.DRIVER) $
          void $
            recomputeOnboardingFlags
              OnboardingFlagsInput
                { ofiPerson =
                    Just
                      PersonFlagsCtx
                        { pfcPerson = person,
                          pfcMerchantOpCityId = merchantOpCityId,
                          pfcMerchantId = merchantId,
                          pfcTransporterConfig = transporterConfig,
                          pfcConfigs = allDocVerificationConfigs,
                          pfcDocs = driverDocuments,
                          pfcVehicleCategory = vehicleCategory,
                          pfcMakeSelfieAadhaarPanMandatory = makeSelfieAadhaarPanMandatory,
                          pfcDriverName = mDL >>= (.driverName),
                          pfcOnboardingVehicleCategory = onboardingVehicleCategory,
                          pfcIsFleetDriver = Nothing,
                          pfcVehicleDocs = vehicleDocumentsUnverified
                        },
                  ofiVehicles = []
                }
              (transporterConfig.unifiedOnboardingFlagsRecompute == Just True)
        -- Vehicle status list (+ vehicle `verified` write, handled inside getVehicleDocuments under enableBotFlow)
        getVehicleDocuments driverDocConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification True driverDocuments merchantOpCityId
      else -- Legacy enablement (unchanged): conditional on separateDriverVehicleEnablement.

        if SDO.isFleetRole person.role || transporterConfig.separateDriverVehicleEnablement == Just True
          then do
            -- Fleet owner enablement/disablement (uses FleetOwnerInformation)
            when (SDO.isFleetRole person.role) $ do
              let vehicleCategory = DVC.CAR
                  allFleetDocsVerified = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
                  isRejectedMandatoryFleetDoc doc =
                    doc.verificationStatus `elem` [FAILED, INVALID]
                      && not (checkIfDocumentValid allDocVerificationConfigs person.role doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory)
              -- First check if fleet should be disabled (has rejected mandatory docs)
              when (any isRejectedMandatoryFleetDoc driverDocuments && transporterConfig.allowDisableFleetOnRejectionDoc == Just True) $
                markDisabledFlags False person FleetRejectionDisable
              -- Then check if fleet should be enabled (all mandatory docs valid)
              when allFleetDocsVerified $
                enableDriver merchantOpCityId personId person.role Nothing transporterConfig merchantId True
            -- Check driver enablement separately (only driver docs + driver inspection)
            when (person.role == DP.DRIVER) $ do
              let vehicleCategory = fromMaybe DVC.CAR $ onboardingVehicleCategory <|> listToMaybe possibleVehicleCategories
                  allDriverDocsVerified = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
              when allDriverDocsVerified $ do
                driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
                let driverInspectionNotRequired = transporterConfig.requiresDriverOnboardingInspection /= Just True || driverInfo.approved == Just True
                    -- Allow first-time auto-enable even when dontAutoEnableDriver=true (enabledAt=Nothing means never enabled before)
                    autoEnableAllowed = not (fromMaybe False transporterConfig.dontAutoEnableDriver) || isNothing driverInfo.enabledAt
                when driverInspectionNotRequired $ do
                  unless driverInfo.verified $ DIQuery.updateVerifiedState (cast personId) True
                  when autoEnableAllowed $ do
                    enableDriver merchantOpCityId personId person.role (mDL >>= (.driverName)) transporterConfig merchantId True
                    whenJust onboardingVehicleCategory $ \category -> do
                      DIIQuery.updateOnboardingVehicleCategory (Just category) personId
            -- Check vehicle enablement separately (only vehicle docs + vehicle inspection)
            getVehicleDocuments driverDocConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification True driverDocuments merchantOpCityId
          else do
            -- Combined enablement: Check both driver and vehicle docs together (old behavior)
            whenJust vehicleCategoryWithoutMandatoryConfigs $ \vehicleCategory -> do
              let allDriverDocsVerified = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
              when (allDriverDocsVerified && transporterConfig.requiresOnboardingInspection /= Just True && person.role == DP.DRIVER) $ do
                driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
                let autoEnableAllowed = not (fromMaybe False transporterConfig.dontAutoEnableDriver) || isNothing driverInfo.enabledAt
                unless driverInfo.verified $ DIQuery.updateVerifiedState (cast personId) True
                when autoEnableAllowed $ do
                  enableDriver merchantOpCityId personId person.role (mDL >>= (.driverName)) transporterConfig merchantId True
                  whenJust onboardingVehicleCategory $ \category -> do
                    DIIQuery.updateOnboardingVehicleCategory (Just category) personId
            -- Check vehicle enablement (old combined logic - checks both driver and vehicle docs)
            getVehicleDocuments driverDocConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification False driverDocuments merchantOpCityId

  (dlDetails, rcDetails) <-
    case prefillData of
      Just True -> do
        let vehRegImgIds = map (.id) $ IQuery.filterImagesByEntityAndType entityImagesInfo merchantOperatingCity.merchantId DVC.VehicleRegistrationCertificate
        dl <- runInReplica $ DLQuery.findByDriverId personId <&> maybeToList
        allRCImgs <- runInReplica $ RCQuery.findAllByImageId vehRegImgIds
        allDLDetails <- mapM convertDLToDLDetails dl
        allRCDetails <- mapM convertRCToRCDetails allRCImgs
        return (Just allDLDetails, Just allRCDetails)
      _ -> return (Nothing, Nothing)

  (enabled, verified, approved, blocked, blockedReason, onboardingAs, disabledReasonFlag) <-
    if SDO.isFleetRole person.role
      then do
        fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
        return (fleetOwnerInfo.enabled, fleetOwnerInfo.verified, fleetOwnerInfo.approved, fleetOwnerInfo.blocked, Nothing, Nothing, fleetOwnerInfo.disabledReasonFlag)
      else do
        driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
        return (driverInfo.enabled, driverInfo.verified, driverInfo.approved, driverInfo.blocked, driverInfo.blockedReason, driverInfo.onboardingAs, driverInfo.disabledReasonFlag)

  recentFleetInfo <-
    if SDO.isFleetRole person.role
      then pure Nothing
      else getRecentFleetDriverAssociationInfo (cast personId)

  digilockerResponseCode <- getDigilockerResponseCode personId

  digilockerAuthorizationUrl <-
    if transporterConfig.digilockerEnabled == Just True
      then SDDigilocker.getDigiLockerAuthorizationUrl personId
      else pure Nothing

  let requestedVehicleDocuments = case mbReqRegistrationNo of
        Nothing -> vehicleDocuments
        Just reqRegistrationNo -> filter (\vehicleDoc -> vehicleDoc.registrationNo == normalizeRegistrationNo reqRegistrationNo) vehicleDocuments

  return $
    StatusRes'
      { driverDocuments,
        vehicleDocuments = requestedVehicleDocuments,
        enabled = enabled,
        verified = verified,
        approved = approved,
        blocked = blocked,
        blockedReason = blockedReason,
        onboardingAs = onboardingAs,
        disabledReasonFlag = disabledReasonFlag,
        recentFleetInfo = recentFleetInfo,
        manualVerificationRequired = transporterConfig.requiresOnboardingInspection,
        driverLicenseDetails = dlDetails,
        vehicleRegistrationCertificateDetails = rcDetails,
        digilockerResponseCode = digilockerResponseCode,
        digilockerAuthorizationUrl = digilockerAuthorizationUrl
      }
  where
    getVehicleDocuments driverDocConfs role vehicleDocumentsUnverified requiresOnboardingInspection vehicleCategoryExcludedFromVerification separateEnablement driverDocuments merchantOpCityId = do
      let personId = person.id
      mbDriverInfo <-
        if role == DP.DRIVER
          then Just <$> (DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId))
          else pure Nothing
      let dontAutoEnable = fromMaybe False entityImagesInfo.transporterConfig.dontAutoEnableDriver
      vehicleDocumentsUnverified `forM` \vehicleDoc@VehicleDocumentItem {..} -> do
        let allVehicleDocsVerified = checkAllVehicleDocsValidForEnabling driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory
            inspectionNotRequired = requiresOnboardingInspection /= Just True || vehicleDoc.isApproved
            isVehicleCategoryExcludedFromVerification = (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) `elem` (fromMaybe [] vehicleCategoryExcludedFromVerification)
            -- When separated: only check vehicle docs. When combined: check both driver and vehicle docs
            allDriverDocsVerified = separateEnablement || checkAllDriverDocsValidForEnabling (Right driverDocConfs) role driverDocuments (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) makeSelfieAadhaarPanMandatory
            -- Vehicle activation logic depends on enablement mode
            checkToActivateRC =
              if separateEnablement
                then (allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification
                else ((allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification) && allDriverDocsVerified

        -- Activate RC if vehicle docs are verified and inspection is not required/approved
        -- isActive=False means RC was explicitly deactivated — skip auto-reactivation for already-enabled drivers.
        -- First-time onboarding still auto-activates: associations are created with isRcActive=False.
        -- Under enableBotFlow: write VRC.verified (= all isMandatory vehicle docs VALID) both ways;
        -- `approved` and RC activation are BOT-owned (suppressed below).
        let enableBotFlow = entityImagesInfo.transporterConfig.enableBotFlow == Just True || entityImagesInfo.transporterConfig.unifiedOnboardingFlagsRecompute == Just True
        -- Routed through the common entry point. Documents are passed through as-is: this site
        -- does NOT waive the InspectionHub gate (the full vehicle recompute does).
        when enableBotFlow $
          void $
            recomputeOnboardingFlags
              OnboardingFlagsInput
                { ofiPerson = Nothing,
                  ofiVehicles =
                    [ VehicleDocsEntry
                        { vdeRegistrationNo = vehicleDoc.registrationNo,
                          vdeItem = vehicleDoc,
                          vdeConfigs = driverDocConfs,
                          vdeMakeSelfieAadhaarPanMandatory = makeSelfieAadhaarPanMandatory
                        }
                    ]
                }
              (entityImagesInfo.transporterConfig.unifiedOnboardingFlagsRecompute == Just True)
        mbVehicle <- QVehicle.findById personId
        let firstTimeOnboarding = maybe False (isNothing . (.enabledAt)) mbDriverInfo
            allowAutoActivate =
              isActive
                || (firstTimeOnboarding && not dontAutoEnable)
        let unifiedRecompute = entityImagesInfo.transporterConfig.unifiedOnboardingFlagsRecompute == Just True
        when (unifiedRecompute && shouldActivateRc && isNothing mbVehicle && checkToActivateRC && role == DP.DRIVER && firstTimeOnboarding) $ do
          void $ withTryCatch "activateRCAutomatically:statusHandler:unifiedRecompute" (activateRCAutomatically personId entityImagesInfo.merchantOperatingCity vehicleDoc.registrationNo)
        when (shouldActivateRc && isNothing mbVehicle && checkToActivateRC && role == DP.DRIVER && not enableBotFlow && allowAutoActivate) $ do
          void $ withTryCatch "activateRCAutomatically:statusHandler" (activateRCAutomatically personId entityImagesInfo.merchantOperatingCity vehicleDoc.registrationNo)
          -- Enable driver when RC is activated (only when flow is NOT separated)
          -- When separated, driver enablement is handled separately in the driver enablement section
          unless separateEnablement $ do
            when (checkToActivateRC && not (fromMaybe False entityImagesInfo.transporterConfig.dontAutoEnableDriver)) $ do
              case (isVehicleCategoryExcludedFromVerification, mDL) of
                (True, _) -> enableDriver merchantOpCityId personId role Nothing entityImagesInfo.transporterConfig entityImagesInfo.merchantOperatingCity.merchantId True
                (False, Just dl) -> enableDriver merchantOpCityId personId role dl.driverName entityImagesInfo.transporterConfig entityImagesInfo.merchantOperatingCity.merchantId True
                (_, _) -> return ()
        if allVehicleDocsVerified then return VehicleDocumentItem {isVerified = True, ..} else return vehicleDoc

    convertDLToDLDetails dl = do
      driverLicenseNumberDec <- decrypt dl.licenseNumber
      let images = entityImagesInfo.entityImages
          mbImage1 = find (\img -> img.id == dl.documentImageId1) images
          mbImage2 = dl.documentImageId2 >>= \imgId2 -> find (\img -> img.id == imgId2) images
          s3Path1 = mbImage1 <&> (.s3Path)
          s3Path2 = mbImage2 <&> (.s3Path)
      pure $
        DLDetails
          { driverName = dl.driverName,
            driverLicenseNumber = driverLicenseNumberDec,
            operatingCity = show entityImagesInfo.merchantOperatingCity.city,
            driverDateOfBirth = dl.driverDob,
            classOfVehicles = dl.classOfVehicles,
            imageId1 = dl.documentImageId1.getId,
            imageId2 = getId <$> dl.documentImageId2,
            createdAt = dl.createdAt,
            dateOfIssue = dl.dateOfIssue,
            s3Path1 = s3Path1,
            s3Path2 = s3Path2,
            documentExpiry = Just dl.licenseExpiry
          }
    convertRCToRCDetails rc = do
      certificateNumberDec <- decrypt rc.certificateNumber
      let mbRcImage = find (\img -> img.id == rc.documentImageId) entityImagesInfo.entityImages
          s3Path = mbRcImage <&> (.s3Path)
      pure $
        RCDetails
          { vehicleRegistrationCertNumber = certificateNumberDec,
            imageId = rc.documentImageId.getId,
            operatingCity = show entityImagesInfo.merchantOperatingCity.city,
            vehicleCategory = show <$> rc.userPassedVehicleCategory,
            airConditioned = rc.airConditioned,
            vehicleManufacturer = rc.vehicleManufacturer,
            vehicleModel = rc.vehicleModel,
            vehicleColor = rc.vehicleColor,
            vehicleDoors = rc.vehicleDoors,
            vehicleSeatBelts = rc.vehicleSeatBelts,
            createdAt = rc.createdAt,
            dateOfRegistration = rc.dateOfRegistration,
            vehicleModelYear = rc.vehicleModelYear,
            oxygen = rc.oxygen,
            ventilator = rc.ventilator,
            failedRules = rc.failedRules,
            verificationStatus = Just rc.verificationStatus,
            s3Path = s3Path,
            documentExpiry = Just rc.fitnessExpiry, -- Fitness expiry = RC expiry
            permitExpiry = rc.permitExpiry
          }

fetchDriverDocuments ::
  OnboardingFlow m r =>
  IQuery.EntityImagesInfo ->
  DocVerificationConfigs ->
  [DVC.VehicleCategory] ->
  DP.Person ->
  Language ->
  Maybe Bool ->
  Maybe Bool ->
  Bool ->
  m [DocumentStatusItem]
fetchDriverDocuments entityImagesInfo allDocVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs skipMessages = do
  let role = person.role
      merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      driverId = person.id
      transporterConfig = entityImagesInfo.transporterConfig
      isDigiLockerEnabled = fromMaybe False transporterConfig.digilockerEnabled
      enableMetadata = entityImagesInfo.enableDocumentMetadata

  digilockerDocStatusMap <- if isDigiLockerEnabled then getDigilockerDocStatusMap driverId else pure DocStatus.emptyDocStatusMap

  driverDocumentTypes <- getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs (transporterConfig.enableBotFlow == Just True || transporterConfig.unifiedOnboardingFlagsRecompute == Just True)
  driverDocumentTypes `forM` \docType -> do
    let mbDocStatus = if isDigiLockerEnabled then DocStatus.getDocStatus docType digilockerDocStatusMap else Nothing
        responseCode = mbDocStatus >>= (.responseCode)
        mbDocVerificationStatus = mbDocStatus >>= (mapDigilockerToResponseStatus . (.status))
    mbCommonDoc <-
      if docType `Set.member` SDO.domainTableDocumentTypes
        then pure Nothing
        else listToMaybe <$> QCommonDocExtra.findLatestByDriverIdAndRcIdAndDocumentType (QCommonDocExtra.OwnedByDriver driverId) docType
    let mbCommonDocData = mbCommonDoc <&> renderCommonDocumentData . (.documentData)

    (mbProcessedStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2, mbMetadata, mbDocumentId) <- getProcessedDriverDocuments person.role person.id entityImagesInfo mbCommonDoc docType useHVSdkForDL enableMetadata
    (status, mbReason, mbUrl, mbExpiryFinal, mbS3PathFinal, mbImageIdFinal, mbImageId2Final, mbDocumentIdFinal) <- case mbProcessedStatus of
      Just VALID -> pure (VALID, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2, mbDocumentId)
      Just s -> pure (s, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2, mbDocumentId)
      Nothing -> case mbDocVerificationStatus of
        Just docStatus -> pure (docStatus, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
        Nothing -> getInProgressDriverDocuments person.role driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs mbCommonDoc

    mbMessage <- documentStatusMessage status mbReason docType mbUrl language skipMessages
    let finalMessage = mbReason <|> (if isDigiLockerEnabled then responseCode else Nothing) <|> mbMessage
    return $ DocumentStatusItem {documentType = docType, documentId = mbDocumentIdFinal, verificationStatus = status, verificationMessage = finalMessage, verificationUrl = mbUrl, s3Path = mbS3PathFinal, imageId = mbImageIdFinal, imageId2 = mbImageId2Final, documentExpiry = mbExpiryFinal, metadata = mbMetadata, commonDocumentData = mbCommonDocData}

getDriverDocTypes ::
  OnboardingFlow m r =>
  Id DMOC.MerchantOperatingCity ->
  DocVerificationConfigs ->
  [DVC.VehicleCategory] ->
  DP.Role ->
  Maybe Bool ->
  Bool ->
  m [DVC.DocumentType]
getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs enableBotFlow = do
  case allDocVerificationConfigs of
    Left fleetConfigs -> do
      -- Fleet-role drift defense: person.role and fleet_owner_information.fleet_type
      -- can drift apart (e.g. role=FLEET_OWNER but fleet_type=BUSINESS_FLEET, so
      -- configs are seeded for FLEET_BUSINESS). For any fleet role, if no configs
      -- match the exact role, fall back to configs for any fleet role in the city.
      let exactRoleConfigs = filter (\config -> config.role == role) fleetConfigs
          anyFleetRoleConfigs = filter (\config -> SDO.isFleetRole config.role) fleetConfigs
          effectiveConfigs =
            if SDO.isFleetRole role && null exactRoleConfigs
              then anyFleetRoleConfigs
              else exactRoleConfigs
          -- BOT broadens the mandatory-fetch set to isMandatoryForEnabling so enabling-only docs
          -- (e.g. fleet OperatorPartnerCode) are fetched for the fleet `enabled` computation; non-BOT
          -- keeps the isMandatory-only set (backward compatible with main).
          mandatoryDocTypes = nub $ map (.documentType) $ filter (\c -> if enableBotFlow then fromMaybe c.isMandatory c.isMandatoryForEnabling else c.isMandatory) effectiveConfigs
          allRoleDocTypes = nub $ map (.documentType) effectiveConfigs
      when (SDO.isFleetRole role && null exactRoleConfigs && not (null anyFleetRoleConfigs)) $
        logInfo $
          "getDriverDocTypes: no fleet configs for role=" <> show role
            <> " in merchantOpCityId="
            <> merchantOpCityId.getId
            <> "; falling back to any-fleet-role configs to mitigate fleet_type/role drift"
      pure $
        if onlyMandatoryDocs == Just True
          then if null mandatoryDocTypes then SDO.defaultFleetDocumentTypes else mandatoryDocTypes
          else if null allRoleDocTypes then SDO.defaultFleetDocumentTypes else allRoleDocTypes
    Right driverConfigs -> do
      let isDriverSideDoc config = isDriverSideDocType config.documentCategory config.documentType
          mandatoryDriverConfigs =
            filter
              ( \config ->
                  fromMaybe config.isMandatory config.isMandatoryForEnabling
                    && config.vehicleCategory `elem` possibleVehicleCategories
                    && isDriverSideDoc config
              )
              driverConfigs
      let allDriverConfigs =
            filter
              ( \config ->
                  config.vehicleCategory `elem` possibleVehicleCategories
                    && isDriverSideDoc config
              )
              driverConfigs
          allDriverDocumentTypes = nub (allDriverConfigs <&> (.documentType))
      if onlyMandatoryDocs == Just True
        then do
          let driverDocumentTypes = nub (mandatoryDriverConfigs <&> (.documentType))
          logInfo $
            "Fetch only mandatory driver docs types: merchantOpCityId: "
              <> merchantOpCityId.getId
              <> "; possibleVehicleCategories: "
              <> show possibleVehicleCategories
              <> "; driverDocumentTypes: "
              <> show driverDocumentTypes
          if null driverDocumentTypes && not (null driverConfigs)
            then do
              logError $
                "getDriverDocTypes: no mandatory driver configs matched for merchantOpCityId="
                  <> merchantOpCityId.getId
                  <> "; possibleVehicleCategories="
                  <> show possibleVehicleCategories
                  <> "; falling back to defaultDriverDocumentTypes rather than treating the empty set as valid"
              pure SDO.defaultDriverDocumentTypes
            else pure driverDocumentTypes
        else pure $ if null allDriverDocumentTypes then SDO.defaultDriverDocumentTypes else allDriverDocumentTypes

-- | All vehicle docs in the enabling set (isMandatoryForEnabling) VALID. Drives @enabled@ / RC activation.
checkAllVehicleDocsValidForEnabling ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValidForEnabling = checkAllVehicleDocsValid' ForEnabling

-- | BOT approve: throws (naming the offending docs) if any BotApproval dependency doc is NOT VALID;
--   otherwise forks the verified/enabled recompute. Throwing before the fork guarantees we never
--   force-enable a fleet/driver whose deps haven't passed.
botApproveAndReconcile ::
  OnboardingFlow m r =>
  DMOC.MerchantOperatingCity ->
  DP.Person ->
  DTC.TransporterConfig ->
  m ()
botApproveAndReconcile merchantOperatingCity person transporterConfig = do
  let language = fromMaybe merchantOperatingCity.language person.language
  (allDocVerificationConfigs, driverDocuments, vehicleCategory, vehicleDocuments) <- fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language (Just True)
  -- BotApproval's dependency docs must be VALID. On the DVC (driver) side a dep counts only if it applies per
  -- `applicableTo` (a fleet driver skips INDIVIDUAL-only deps like OperatorPartnerCode); FleetOwnerDVC has no split.
  isFleetDriver <- case allDocVerificationConfigs of
    Right _ -> hasActiveFleetAssociation person.id
    Left _ -> pure False
  let invalidDeps = case allDocVerificationConfigs of
        Left fleetConfigs ->
          -- Role-aware: pick the BotApproval row for this person's role, not just any role's row.
          invalidDependencyDocs Nothing [] (maybe [] (.dependencyDocumentType) $ findFleetConfigForRole DVC.BotApproval person.role fleetConfigs) driverDocuments
        Right driverConfigs ->
          invalidDependencyDocs (Just isFleetDriver) driverConfigs (maybe [] (.dependencyDocumentType) $ find (\c -> c.documentType == DVC.BotApproval) driverConfigs) driverDocuments
  -- Block approval (and the fork below) when any dependency doc isn't VALID — surface which docs failed.
  unless (null invalidDeps) $
    throwError (InvalidRequest $ "Cannot approve: BotApproval dependency documents not valid: " <> T.intercalate ", " (map show invalidDeps))
  fork "botApproveAndReconcile: recompute verified/enabled" $
    void $
      Hedis.withLockRedisAndReturnValue (mkPersonDocsStatusKey person.id) onboardingLockTTLSeconds $ do
        let docs' = map forceBotApprovalDocValid driverDocuments
            useUnifiedOnboardingFlagsRecompute = transporterConfig.unifiedOnboardingFlagsRecompute == Just True
        -- Fleet-owner vs driver is picked inside recomputeOnboardingFlags from the config source.
        void $
          recomputeOnboardingFlags
            OnboardingFlagsInput
              { ofiPerson =
                  Just
                    PersonFlagsCtx
                      { pfcPerson = person,
                        pfcMerchantOpCityId = merchantOperatingCity.id,
                        pfcMerchantId = merchantOperatingCity.merchantId,
                        pfcTransporterConfig = transporterConfig,
                        pfcConfigs = allDocVerificationConfigs,
                        pfcDocs = docs',
                        pfcVehicleCategory = vehicleCategory,
                        pfcMakeSelfieAadhaarPanMandatory = Nothing,
                        pfcDriverName = Nothing,
                        pfcOnboardingVehicleCategory = Nothing,
                        pfcIsFleetDriver = Just isFleetDriver,
                        pfcVehicleDocs = vehicleDocuments
                      },
                ofiVehicles = []
              }
            useUnifiedOnboardingFlagsRecompute

-- | Throw InvalidRequest if any driver under the fleet has a NEW or
--   INPROGRESS ride. Used as a guard before flipping fleet enabled to false.
ensureNoActiveRidesUnderFleet :: OnboardingFlow m r => Id DP.Person -> m ()
ensureNoActiveRidesUnderFleet fleetOwnerId = do
  driverIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId.getId
  anyActive <- anyM (fmap isJust . QRideExtra.getUpcomingOrActiveByDriverId) driverIds
  when anyActive $
    throwError $ InvalidRequest "Cannot disable fleet: one or more drivers have active rides"

activateRCAutomatically :: OnboardingFlow m r => Id DP.Person -> DMOC.MerchantOperatingCity -> Text -> m ()
activateRCAutomatically personId merchantOpCity rcNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantOpCity.merchantId, merchantOpCity.id) False rcStatusReq

mkDLMetadata :: OnboardingFlow m r => Maybe DL.DriverLicense -> m (Maybe DocumentMetadata)
mkDLMetadata mbDl = forM mbDl $ \dl -> do
  licenseNumberDec <- decrypt dl.licenseNumber
  pure $ DLMetadata DLDocumentMetadata {driverLicenseNumber = licenseNumberDec, driverDateOfBirth = dl.driverDob, dateOfExpiry = dl.licenseExpiry, imageId1 = Just dl.documentImageId1.getId, imageId2 = dl.documentImageId2 <&> (.getId)}

mkAadhaarMetadata :: OnboardingFlow m r => Maybe DAadhaarCard.AadhaarCard -> m (Maybe DocumentMetadata)
mkAadhaarMetadata mbAadhaarCard = forM mbAadhaarCard $ \aadhaar -> do
  aadhaarNumberDec <- mapM decrypt aadhaar.aadhaarNumber
  pure $ AadhaarMetadata AadhaarDocumentMetadata {aadhaarNumber = aadhaarNumberDec, nameOnCard = aadhaar.nameOnCard, dateOfBirth = aadhaar.dateOfBirth, address = aadhaar.address}

mkPanMetadata :: OnboardingFlow m r => Maybe DPan.DriverPanCard -> m (Maybe DocumentMetadata)
mkPanMetadata mbPanCard = forM mbPanCard $ \pan -> do
  panNumberDec <- decrypt pan.panCardNumber
  pure $ PanMetadata PanDocumentMetadata {panNumber = panNumberDec, panDocType = pan.docType, driverDob = pan.driverDob}

mkGSTMetadata :: OnboardingFlow m r => Maybe DGstin.DriverGstin -> m (Maybe DocumentMetadata)
mkGSTMetadata mbGSTCertificate = forM mbGSTCertificate $ \gst -> do
  gstNumberDec <- decrypt gst.gstin
  pure $ GSTMetadata GSTDocumentMetadata {gstNumber = gstNumberDec}

mkUDYAMMetadata :: OnboardingFlow m r => Id DP.Person -> Maybe DUdyam.DriverUdyam -> m (Maybe DocumentMetadata)
mkUDYAMMetadata driverIdForFoi mbUdyam = forM mbUdyam $ \udyam -> do
  udyamNumberDec <- decrypt udyam.udyamNumber
  mbFoi <- QFOI.findByPrimaryKey driverIdForFoi
  pure $ UDYAMMetadata UDYAMDocumentMetadata {udyamNumber = Just udyamNumberDec, tdsRate = mbFoi >>= (.tdsRate)}

getProcessedDriverDocuments :: OnboardingFlow m r => DP.Role -> Id DP.Person -> IQuery.EntityImagesInfo -> Maybe DCDOD.CommonDriverOnboardingDocuments -> DVC.DocumentType -> Maybe Bool -> Bool -> m (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text, Maybe DocumentMetadata, Maybe Text)
getProcessedDriverDocuments role driverId entityImagesInfo mbCommonDoc docType useHVSdkForDL enableMetadata = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      (mbS3Path, mbImageId) = getImageMetaFromLatestImage entityImagesInfo docType
      lookupImage imgId =
        let mbImg = find (\img -> img.id == imgId) entityImagesInfo.entityImages
            iid = Just imgId.getId
         in (mbImg <&> (.s3Path), iid)
      lookupImageFailReason imgId =
        extractImageFailReason (find (\img -> img.id == imgId) entityImagesInfo.entityImages >>= (.failureReason))
  withCommonDocumentStatus entityImagesInfo mbCommonDoc (mbS3Path, mbImageId) fromCommonDoc $
    case docType of
      DVC.DriverLicense -> do
        mbDL <- DLQuery.findByDriverId driverId -- add failure reason in dl and rc
        if isNothing mbDL && (useHVSdkForDL == Just True)
          then do
            void $ withTryCatch "callGetDLGetStatus:getProcessedDriverDocuments" $ callGetDLGetStatus driverId merchantOpCityId
            mbDL' <- DLQuery.findByDriverId driverId
            -- Expiry from DL table's licenseExpiry field (not from Image table)
            let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbDL'
                iid2 = mbDL' >>= (.documentImageId2) <&> (.getId)
                reason = (mbDL' >>= (.rejectReason)) <|> (if (mbDL' <&> (.verificationStatus)) == Just Documents.INVALID then (mbDL' <&> (.documentImageId1)) >>= lookupImageFailReason else Nothing)
            mbDlMetadata <- if enableMetadata then mkDLMetadata mbDL' else pure Nothing
            return (mapStatus <$> (mbDL' <&> (.verificationStatus)), reason, Nothing, mbDL' <&> (.licenseExpiry), s3, iid, iid2, mbDlMetadata, mbDL' <&> (.id.getId))
          else do
            let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbDL
                iid2 = mbDL >>= (.documentImageId2) <&> (.getId)
                reason = (mbDL >>= (.rejectReason)) <|> (if (mbDL <&> (.verificationStatus)) == Just Documents.INVALID then (mbDL <&> (.documentImageId1)) >>= lookupImageFailReason else Nothing)
            mbDlMetadata <- if enableMetadata then mkDLMetadata mbDL else pure Nothing
            return (mapStatus <$> (mbDL <&> (.verificationStatus)), reason, Nothing, mbDL <&> (.licenseExpiry), s3, iid, iid2, mbDlMetadata, mbDL <&> (.id.getId))
      DVC.AadhaarCard -> do
        mbAadhaarCard <- QAadhaarCard.findByPrimaryKey driverId
        let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage (mbAadhaarCard >>= (.aadhaarFrontImageId))
            iid2 = mbAadhaarCard >>= (.aadhaarBackImageId) <&> (.getId)
            reason = if (mbAadhaarCard <&> (.verificationStatus)) == Just Documents.INVALID then ((mbAadhaarCard >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbAadhaarCard >>= (.aadhaarFrontImageId)) >>= lookupImageFailReason) else Nothing
        mbAadhaarMetadata <- if enableMetadata then mkAadhaarMetadata mbAadhaarCard else pure Nothing
        return (mapStatus . (.verificationStatus) <$> mbAadhaarCard, reason, Nothing, Nothing, s3, iid, iid2, mbAadhaarMetadata, mbAadhaarCard <&> (.driverId.getId))
      DVC.Permissions -> return (Just VALID, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.SocialSecurityNumber -> do
        mbSSN <- QDSSN.findByDriverId driverId
        return (mapStatus <$> (mbSSN <&> (.verificationStatus)), mbSSN >>= (.rejectReason), Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.PanCard -> do
        mbPanCard <- QDPC.findByDriverId driverId
        let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbPanCard
            iid2 = mbPanCard >>= (.documentImageId2) <&> (.getId)
            reason = if (mbPanCard <&> (.verificationStatus)) == Just Documents.INVALID then ((mbPanCard >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbPanCard <&> (.documentImageId1)) >>= lookupImageFailReason) else Nothing
        mbPanMetadata <- if enableMetadata then mkPanMetadata mbPanCard else pure Nothing
        return (mapStatus . (.verificationStatus) <$> mbPanCard, reason, Nothing, Nothing, s3, iid, iid2, mbPanMetadata, mbPanCard <&> (.id.getId))
      DVC.GSTCertificate -> do
        mbGSTCertificate <- QDGST.findByDriverId driverId
        let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbGSTCertificate
            iid2 = mbGSTCertificate >>= (.documentImageId2) <&> (.getId)
            reason = if (mbGSTCertificate <&> (.verificationStatus)) == Just Documents.INVALID then ((mbGSTCertificate >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbGSTCertificate <&> (.documentImageId1)) >>= lookupImageFailReason) else Nothing
        mbGstMetadata <- if enableMetadata then mkGSTMetadata mbGSTCertificate else pure Nothing
        return (mapStatus . (.verificationStatus) <$> mbGSTCertificate, reason, Nothing, Nothing, s3, iid, iid2, mbGstMetadata, mbGSTCertificate <&> (.id.getId))
      DVC.BackgroundVerification -> do
        mbBackgroundVerification <- BVQuery.findByDriverId driverId
        -- Expiry from BackgroundVerification table's expiresAt field (not from Image table)
        if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
          then return (Just VALID, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing, Nothing, Nothing)
          else return (Nothing, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.LocalResidenceProof -> do
        let (status, reason, url) = checkImageValidity entityImagesInfo DVC.LocalResidenceProof
        -- Fleet owners store the local address triplet in fleet_owner_information; drivers in driver_identity_info.
        mbAddressFields <-
          if SDO.isFleetRole role
            then do
              mbFleetInfo <- QFOI.findByPrimaryKey driverId
              pure $ mbFleetInfo <&> \info -> (info.address, info.addressState, info.addressDocumentType)
            else do
              mbIdentityInfo <- QDII.findByDriverId driverId
              driverInfo <- DIQuery.findById (cast driverId) >>= fromMaybeM (PersonNotFound driverId.getId)
              let idInfo = DIInfo.getIdentityInfo mbIdentityInfo driverInfo
              pure $ Just (idInfo.address, idInfo.addressState, idInfo.addressDocumentType)
        let hasAddressDetails =
              maybe False (\(address, addressState, addressDocumentType) -> isJust address && isJust addressDocumentType && isJust addressState) mbAddressFields
        let mbLocalMetadata =
              if enableMetadata
                then
                  mbAddressFields <&> \(address, addressState, addressDocumentType) ->
                    LocalAddressProofMetadata LocalAddressProofDocumentMetadata {state = addressState, proofDocumentType = addressDocumentType, address = address}
                else Nothing
        let finalStatus = if hasAddressDetails then status else if isJust mbImageId then Just INVALID else Just NO_DOC_AVAILABLE
            rejectReason = if finalStatus == Just INVALID then (Id <$> mbImageId) >>= lookupImageFailReason else Nothing
        return (finalStatus, reason <|> rejectReason, url, Nothing, mbS3Path, mbImageId, Nothing, mbLocalMetadata, Nothing)
      DVC.DriverVehicleNOC -> do
        let (status, reason, url) = checkImageValidity entityImagesInfo DVC.DriverVehicleNOC
        return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.TrainingForm -> do
        status <- checkLMSTrainingStatus driverId merchantOpCityId
        return (status, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.DriverInspectionHub -> do
        (status, reason) <- getInspectionHubStatusAndReason DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
        return (status, reason, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      DVC.OperatorPartnerCode -> do
        status <- getOperatorPartnerCodeStatus role driverId
        return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      DVC.BotApproval -> do
        status <- getBotApprovalStatusForPerson role driverId
        return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      DVC.UDYAMCertificate -> do
        mbUdyam <- QUDYAM.findByDriverId driverId
        case mbUdyam of
          Just udyam -> do
            mbUdyamMetadata <- if enableMetadata then mkUDYAMMetadata driverId (Just udyam) else pure Nothing
            return (Just $ mapStatus udyam.verificationStatus, udyam.rejectReason, Nothing, Nothing, mbS3Path, mbImageId, Nothing, mbUdyamMetadata, Just udyam.id.getId)
          Nothing -> do
            let hasImage = not . null $ IQuery.filterImageByEntityIdAndImageTypeAndVerificationStatus entityImagesInfo DVC.UDYAMCertificate [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
            return (if hasImage then Just MANUAL_VERIFICATION_REQUIRED else Nothing, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
      DVC.TANCertificate -> do
        let (status, reason, url) = checkImageValidity entityImagesInfo DVC.TANCertificate
        mbTanMetadata <-
          if enableMetadata
            then do
              mbFoi <- QFOI.findByPrimaryKey driverId
              pure $ Just $ TANMetadata TANDocumentMetadata {tdsRate = mbFoi >>= (.tdsRate)}
            else pure Nothing
        return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, mbTanMetadata, Nothing)
      DVC.LDCCertificate -> do
        let (status, reason, url) = checkImageValidity entityImagesInfo DVC.LDCCertificate
        mbLdcMetadata <-
          if enableMetadata
            then do
              mbFoi <- QFOI.findByPrimaryKey driverId
              pure $ Just $ LDCMetadata LDCDocumentMetadata {tdsRate = mbFoi >>= (.tdsRate)}
            else pure Nothing
        return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, mbLdcMetadata, Nothing)
      DVC.NomineeDetails -> do
        mbIdentityInfo <- QDII.findByDriverId driverId
        let hasNominee = maybe False (\info -> isJust info.nomineeName && isJust info.nomineeRelationship && isJust info.nomineeDob) mbIdentityInfo
            mbNomineeMetadata =
              if enableMetadata
                then
                  mbIdentityInfo <&> \info ->
                    NomineeDetailsMetadata NomineeDetailsDocumentMetadata {nomineeName = info.nomineeName, nomineeDob = info.nomineeDob, nomineeRelationship = info.nomineeRelationship}
                else Nothing
        return (if hasNominee then Just VALID else Nothing, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, mbNomineeMetadata, Nothing)
      DVC.FleetRegistration -> do
        mbRegisteredAt <-
          if SDO.isFleetRole role
            then (.registeredAt) <$> (QFOI.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound driverId.getId))
            else pure Nothing
        return (VALID <$ mbRegisteredAt, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
      DVC.BankingDetails -> do
        mbDriverInfo <- DIQuery.findById (cast driverId)
        unless (maybe False (.enabled) mbDriverInfo) $
          void $
            withTryCatch "getPersonRegisterBankAccountStatus:getProcessedDriverDocuments" $
              SPBA.getPersonRegisterBankAccountStatus (Just True) driverId merchantOpCityId
        mbBankAccount <- QDriverBankAccount.findByPrimaryKey driverId
        bankingDetailsConfigs <- CQDVC.findByMerchantOpCityIdAndDocumentType merchantOpCityId DVC.BankingDetails Nothing
        let isManualVerification = maybe False (.doStrictVerifcation) (listToMaybe bankingDetailsConfigs)
            mbChargesEnabled = (.chargesEnabled) <$> mbBankAccount
            mbPayoutsEnabled = mbBankAccount >>= (.payoutsEnabled)
            mbDetailsSubmitted = (.detailsSubmitted) <$> mbBankAccount
            bankAccountStatus
              | mbChargesEnabled == Just True && mbPayoutsEnabled == Just True = Just VALID
              | mbDetailsSubmitted == Just True =
                if isManualVerification then Just MANUAL_VERIFICATION_REQUIRED else Just PENDING
              | otherwise = Nothing
            mkBankingMetadata accountNumber ifscCode nameAtBank upiId =
              BankingDetailsMetadata
                BankingDetailsDocumentMetadata
                  { accountNumber,
                    ifscCode,
                    nameAtBank,
                    upiId,
                    chargesEnabled = mbChargesEnabled,
                    payoutsEnabled = mbPayoutsEnabled,
                    detailsSubmitted = mbDetailsSubmitted
                  }
        (hasBankingDetails, mbBankingMetadata) <-
          if SDO.isFleetRole role
            then do
              mbFleetInfo <- QFOI.findByPrimaryKey driverId
              return
                ( maybe False (isJust . (.payoutVpa)) mbFleetInfo,
                  if enableMetadata
                    then mbFleetInfo <&> \fi -> mkBankingMetadata fi.payoutVpaBankAccount Nothing Nothing fi.payoutVpa
                    else Nothing
                )
            else do
              return
                ( maybe False (\di -> isJust di.driverBankAccountDetails || isJust di.payerVpa) mbDriverInfo,
                  if enableMetadata
                    then
                      mbDriverInfo <&> \di ->
                        mkBankingMetadata
                          (di.driverBankAccountDetails >>= (.accountNumber))
                          (di.driverBankAccountDetails >>= (.ifscCode))
                          (di.driverBankAccountDetails >>= (.nameAtBank))
                          di.payerVpa
                    else Nothing
                )
        return (bankAccountStatus <|> (if hasBankingDetails then Just VALID else Nothing), Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, mbBankingMetadata, Nothing)
      _ -> do
        let (status, reason, url) = checkImageValidity entityImagesInfo docType
        return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing, Nothing)
  where
    fromCommonDoc res = (Just res.status, res.reason, Nothing, Nothing, res.s3Path, res.imageId, Nothing, Nothing, res.documentId)

callGetDLGetStatus :: OnboardingFlow m r => Id DP.Person -> Id DMOC.MerchantOperatingCity -> m ()
callGetDLGetStatus driverId merchantOpCityId = do
  latestReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId DVC.DriverLicense
  whenJust latestReq $ \verificationReq -> do
    when (verificationReq.status == "pending" || verificationReq.status == "source_down_retrying") $ do
      -- statusHandler reaches this twice per render (getDLAndStatus + getProcessedDriverDocuments) and the
      -- app polls status: dedupe to one getTask per requestId per window (key shared with reconcilePending).
      firstPullInWindow <- Hedis.setNxExpire (SDO.getTaskPullKey verificationReq.requestId) (round SDO.getTaskPullWindow) ()
      when firstPullInWindow $ do
        allowed <- SDO.allowGetTaskAttempt verificationReq.requestId
        when allowed $ do
          rsp <- Verification.getTask merchantOpCityId KEV.HyperVergeRCDL (KEV.GetTaskReq (Just "checkDL") verificationReq.requestId) HVQuery.updateResponse
          case rsp of
            KEV.DLResp resp -> do
              logDebug $ "callGetDLGetStatus: getTask api response for request id : " <> verificationReq.requestId <> " is : " <> show resp
              unless ("still being processed" `T.isInfixOf` (fromMaybe "" resp.message)) (void $ DDL.onVerifyDL (SDO.makeHVVerificationReqRecord verificationReq) resp KEV.HyperVergeRCDL)
            _ -> throwError $ InternalError "Document and apiEndpoint mismatch occurred !!!!!!!!"

checkImageValidity :: IQuery.EntityImagesInfo -> DVC.DocumentType -> (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl)
checkImageValidity entityImagesInfo docType = do
  let validImages = IQuery.filterImageByEntityIdAndImageTypeAndVerificationStatus entityImagesInfo docType [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
  checkValidity validImages
  where
    checkValidity validImages
      | any (\img -> img.verificationStatus == Just Documents.VALID) validImages = (Just VALID, Nothing, Nothing)
      | any (\img -> img.verificationStatus == Just Documents.MANUAL_VERIFICATION_REQUIRED) validImages = (Just MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)
      | otherwise = (Nothing, Nothing, Nothing)

checkLMSTrainingStatus ::
  ( ServiceFlow m r,
    EsqDBFlow m r,
    CacheFlow m r
  ) =>
  Id DP.Person ->
  Id DMOC.MerchantOperatingCity ->
  m (Maybe ResponseStatus)
checkLMSTrainingStatus driverId merchantOpCityId = do
  hasCompleted <- TPlasma.allLMSTrainingCompleted merchantOpCityId (driverId.getId)
  return $ hasCompleted >>= (\ok -> if ok then Just VALID else Nothing)

-- | OperatorPartnerCode status by role: fleet reads fleet_operator_association, driver reads driver_operator_association.
getOperatorPartnerCodeStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => DP.Role -> Id DP.Person -> m (Maybe ResponseStatus)
getOperatorPartnerCodeStatus role personId
  | SDO.isFleetRole role = getOperatorPartnerCodeStatusForFleet personId
  | otherwise = getOperatorPartnerCodeStatusForDriver personId

-- | Driver OperatorPartnerCode status, derived from the active driver-operator association
--   (activated by postOperatorConsent).
getOperatorPartnerCodeStatusForDriver :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> m (Maybe ResponseStatus)
getOperatorPartnerCodeStatusForDriver driverId = do
  mbAssoc <- QDOA.findByDriverId driverId True
  pure $ VALID <$ mbAssoc

-- | Fleet OperatorPartnerCode status from the active fleet-operator association.
getOperatorPartnerCodeStatusForFleet :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Id DP.Person -> m (Maybe ResponseStatus)
getOperatorPartnerCodeStatusForFleet fleetOwnerId = do
  mbAssoc <- QFOA.findActiveByFleetOwnerId fleetOwnerId
  pure $ VALID <$ mbAssoc

checkBackgroundVerificationStatus :: OnboardingFlow m r => Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkBackgroundVerificationStatus driverId merchantId merchantOpCityId = do
  mbBackgroundVerification <- BVQuery.findByDriverId driverId
  case mbBackgroundVerification of
    Just backgroundVerification -> do
      now <- getCurrentTime
      if now >= backgroundVerification.expiresAt
        then return (NO_DOC_AVAILABLE, Nothing, Nothing)
        else do
          invitation <- BackgroundVerification.getInvitation merchantId merchantOpCityId backgroundVerification.invitationId
          case invitation.status of
            "completed" -> do
              BVQuery.updateInvitationStatus Documents.VALID driverId
              case invitation.reportId of
                Just reportId -> do
                  BVQuery.updateReportId (Just reportId) driverId
                  report <- BackgroundVerification.getReport merchantId merchantOpCityId reportId
                  case report.status of
                    "complete" -> do
                      case (report.assessment, report.adjudication) of
                        (Just "eligible", _) -> do
                          BVQuery.updateReportStatus Documents.VALID driverId
                          return (VALID, Nothing, Nothing)
                        (_, Just "engaged") -> do
                          BVQuery.updateReportStatus Documents.VALID driverId
                          return (VALID, Nothing, Nothing)
                        (_, Just "post_adverse_action") -> do
                          BVQuery.updateReportStatus Documents.UNAUTHORIZED driverId
                          return (UNAUTHORIZED, Nothing, Nothing)
                        (_, _) -> return (PENDING, Nothing, Nothing)
                    "pending" -> return (PENDING, Nothing, Nothing)
                    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
                Nothing -> return (PENDING, Nothing, Nothing)
            "pending" -> return (PENDING, Nothing, Just invitation.invitationUrl)
            _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing, Nothing)

getInProgressDriverDocuments ::
  OnboardingFlow m r =>
  DP.Role ->
  Id DP.Person ->
  IQuery.EntityImagesInfo ->
  DDVC.DocumentType ->
  [DVC.VehicleCategory] ->
  DocVerificationConfigs ->
  Maybe DCDOD.CommonDriverOnboardingDocuments ->
  m (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text, Maybe Text)
getInProgressDriverDocuments role driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs mbCommonDoc = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      merchantId = entityImagesInfo.merchantOperatingCity.merchantId
      (mbS3Path, mbImageId) = getImageMetaFromLatestImage entityImagesInfo docType
      filteredDocVerificationConfigs =
        case allDocVerificationConfigs of
          Left fleetConfs -> Left fleetConfs
          Right driverConfs -> Right $ filter (\c -> c.vehicleCategory `elem` possibleVehicleCategories) driverConfs
      onlyImageLookup = case allDocVerificationConfigs of
        Right driverConfs ->
          let relevantDriverConfs = filter (\c -> c.vehicleCategory `elem` possibleVehicleCategories) driverConfs
           in maybe False (fromMaybe False . (.onlyImageVerificationStatusLookupRequired)) $ find (\c -> c.documentType == docType) relevantDriverConfs
        Left fleetConfs ->
          maybe False (fromMaybe False . (.onlyImageVerificationStatusLookupRequired)) $ find (\c -> c.documentType == docType) fleetConfs
  withCommonDocumentStatus entityImagesInfo mbCommonDoc (mbS3Path, mbImageId) fromCommonDoc $ do
    (status, mbReason, mbUrl) <- case docType of
      DDVC.DriverLicense -> checkIfUnderProgress entityImagesInfo DDVC.DriverLicense
      DDVC.BackgroundVerification -> checkBackgroundVerificationStatus driverId merchantId merchantOpCityId
      DDVC.AadhaarCard -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.AadhaarCard onlyImageLookup filteredDocVerificationConfigs
      DDVC.PanCard -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.PanCard onlyImageLookup filteredDocVerificationConfigs
      DDVC.GSTCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.GSTCertificate onlyImageLookup filteredDocVerificationConfigs
      DDVC.Permissions -> return (VALID, Nothing, Nothing)
      DDVC.ProfilePhoto -> do
        let mbImages = IQuery.filterRecentLatestByEntityIdAndImageType entityImagesInfo DDVC.ProfilePhoto
            profileStatus = maybe NO_DOC_AVAILABLE mapStatus (mbImages >>= (.verificationStatus))
            profileReason = if profileStatus == INVALID then extractImageFailReason (mbImages >>= (.failureReason)) else Nothing
        return (profileStatus, profileReason, Nothing)
      DDVC.UploadProfile -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.UploadProfile onlyImageLookup filteredDocVerificationConfigs
      DDVC.DrivingSchoolCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.DrivingSchoolCertificate onlyImageLookup filteredDocVerificationConfigs
      DDVC.PoliceVerificationCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.PoliceVerificationCertificate onlyImageLookup filteredDocVerificationConfigs
      DDVC.LocalResidenceProof -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.LocalResidenceProof onlyImageLookup filteredDocVerificationConfigs
      DDVC.DriverVehicleNOC -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.DriverVehicleNOC onlyImageLookup filteredDocVerificationConfigs
      DDVC.TrainingForm -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.TrainingForm onlyImageLookup filteredDocVerificationConfigs
      DDVC.DriverInspectionHub -> do
        (mbStatus, reason) <- getInspectionHubStatusAndReason DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
        let status = fromMaybe INVALID mbStatus
        return (status, reason, Nothing)
      DDVC.OperatorPartnerCode -> do
        mbStatus <- getOperatorPartnerCodeStatus role driverId
        return (fromMaybe NO_DOC_AVAILABLE mbStatus, Nothing, Nothing)
      DDVC.BotApproval -> do
        mbStatus <- getBotApprovalStatusForPerson role driverId
        return (fromMaybe NO_DOC_AVAILABLE mbStatus, Nothing, Nothing)
      DDVC.BusinessLicense -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.BusinessLicense onlyImageLookup filteredDocVerificationConfigs
      DDVC.TaxiTransportLicense -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.TaxiTransportLicense onlyImageLookup filteredDocVerificationConfigs
      DDVC.BusinessRegistrationExtract -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.BusinessRegistrationExtract onlyImageLookup filteredDocVerificationConfigs
      DDVC.TAXDetails -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.TAXDetails onlyImageLookup filteredDocVerificationConfigs
      DDVC.FinnishIDResidencePermit -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.FinnishIDResidencePermit onlyImageLookup filteredDocVerificationConfigs
      DDVC.TANCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.TANCertificate onlyImageLookup filteredDocVerificationConfigs
      DDVC.LDCCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.LDCCertificate onlyImageLookup filteredDocVerificationConfigs
      _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    return (status, mbReason, mbUrl, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
  where
    fromCommonDoc res = (res.status, res.reason, Nothing, Nothing, res.s3Path, res.imageId, Nothing, res.documentId)

checkIfImageUploadedOrInvalidated :: OnboardingFlow m r => DP.Role -> IQuery.EntityImagesInfo -> DDVC.DocumentType -> Bool -> DocVerificationConfigs -> m (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated role entityImagesInfo docType onlyImageLookup allDocVerificationConfigs = do
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
      hasDocumentVerificationConfig =
        case allDocVerificationConfigs of
          Left fleetConfigs ->
            -- Per-docType role match for fleet roles; fall back to any config row for this docType (old behavior).
            let exactRoleConfigs = filter (\c -> c.documentType == docType && c.role == role) fleetConfigs
                fallbackConfigs = filter (\c -> c.documentType == docType) fleetConfigs
                effectiveConfigs = if SDO.isFleetRole role && not (null exactRoleConfigs) then exactRoleConfigs else fallbackConfigs
             in any
                  (\config -> not config.isDefaultEnabledOnManualVerification)
                  effectiveConfigs
          Right driverConfigs ->
            any
              ( \config ->
                  config.documentType == docType
                    && not config.isDefaultEnabledOnManualVerification
              )
              driverConfigs
  case images of
    [] -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
    latestImage : _ -> do
      case latestImage.verificationStatus of
        Just Documents.VALID | onlyImageLookup -> return (VALID, Nothing, Nothing)
        Just Documents.INVALID -> return (INVALID, extractImageFailReason latestImage.failureReason, Nothing)
        _ ->
          if hasDocumentVerificationConfig
            then return (FAILED, Nothing, Nothing)
            else return (MANUAL_VERIFICATION_REQUIRED, Nothing, Nothing)

getAadhaarStatus :: OnboardingFlow m r => Id DP.Person -> m (ResponseStatus, Maybe DAadhaarCard.AadhaarCard)
getAadhaarStatus personId = do
  mAadhaarCard <- QAadhaarCard.findByPrimaryKey personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.verificationStatus == Documents.VALID
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: OnboardingFlow m r => Id DP.Person -> IQuery.EntityImagesInfo -> Language -> Maybe Bool -> m (ResponseStatus, Maybe DL.DriverLicense, Text)
getDLAndStatus driverId entityImagesInfo language useHVSdkForDL = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
  mDriverLicense <- do
    mbDL' <- DLQuery.findByDriverId driverId
    case mbDL' of
      Just dl -> return $ Just dl
      Nothing -> do
        if useHVSdkForDL == Just True
          then do
            void $ withTryCatch "callGetDLGetStatus:getDLAndStatus" $ callGetDLGetStatus driverId merchantOpCityId
            DLQuery.findByDriverId driverId
          else return Nothing
  (status, message) <-
    case mDriverLicense of
      Just driverLicense -> do
        let status = mapStatus driverLicense.verificationStatus
        msg <- verificationStatusCheck status language DVC.DriverLicense Nothing
        return (status, msg)
      Nothing -> do
        (status, message) <- checkIfInVerification driverId entityImagesInfo DVC.DriverLicense language
        return (status, message)
  return (status, mDriverLicense, message)

getRCAndStatus :: OnboardingFlow m r => Id DP.Person -> IQuery.EntityImagesInfo -> Language -> m (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
getRCAndStatus driverId entityImagesInfo language = do
  associations <- DRAQuery.findAllLinkedByDriverId driverId
  if null associations
    then do
      (status, message) <- checkIfInVerification driverId entityImagesInfo DVC.VehicleRegistrationCertificate language
      return (status, Nothing, message)
    else do
      mVehicleRCs <- RCQuery.findById `mapM` ((.rcId) <$> associations)
      let vehicleRCs = catMaybes mVehicleRCs
      let mValidVehicleRC = find (\rc -> rc.verificationStatus == Documents.VALID) vehicleRCs
      case mValidVehicleRC of
        Just validVehicleRC -> do
          msg <- toVerificationMessage DocumentValid language
          return (VALID, Just validVehicleRC, msg)
        Nothing -> do
          let mVehicleRC = listToMaybe vehicleRCs
          case mVehicleRC of
            Just vehicleRC -> do
              let status = mapStatus vehicleRC.verificationStatus
              message <- verificationStatusCheck status language DVC.VehicleRegistrationCertificate (Just vehicleRC.failedRules)
              return (status, Just vehicleRC, message)
            Nothing -> do
              msg <- toVerificationMessage NoDcoumentFound language
              return (NO_DOC_AVAILABLE, Nothing, msg)

verificationStatusCheck :: OnboardingFlow m r => ResponseStatus -> Language -> DVC.DocumentType -> Maybe [Text] -> m Text
verificationStatusCheck status language img mbReasons = do
  case (status, img) of
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> do
      msg <- toVerificationMessage RCInvalid language
      addVerificationReasons language mbReasons msg
    _ -> toVerificationMessage DocumentValid language

addVerificationReasons :: OnboardingFlow m r => Language -> Maybe [Text] -> Text -> m Text
addVerificationReasons language mbReasons msg = do
  case mbReasons of
    Just reasons | not (null reasons) -> do
      translatedReasons <- forM reasons $ \reason -> do
        let (key, value) = T.breakOn ":" reason
        translatedKey <- translateDynamicKey key language
        if T.null value
          then pure translatedKey
          else pure $ translatedKey <> ": " <> T.drop 1 value
      pure $ msg <> T.intercalate ", " translatedReasons
    _ -> pure msg

checkIfInVerification :: OnboardingFlow m r => Id DP.Person -> IQuery.EntityImagesInfo -> DVC.DocumentType -> Language -> m (ResponseStatus, Text)
checkIfInVerification driverId entityImagesInfo docType language = do
  let onboardingTryLimit = entityImagesInfo.transporterConfig.onboardingTryLimit
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId (docTypeToText docType)
  hvVerificationReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord idfyVerificationReq hvVerificationReq
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
  verificationStatusWithMessage onboardingTryLimit (length images) mbVerificationReqRecord language docType

verificationStatusWithMessage :: OnboardingFlow m r => Int -> Int -> Maybe SDO.VerificationReqRecord -> Language -> DVC.DocumentType -> m (ResponseStatus, Text)
verificationStatusWithMessage onboardingTryLimit imagesNum mbVerificationReqRecord language docType =
  case mbVerificationReqRecord of
    Just req -> do
      mbRC <- case docType of
        DVC.VehicleRegistrationCertificate -> do
          registrationNoEither <- withTryCatch "decryptDocumentNumber:verificationStatusWithMessage" (decrypt req.documentNumber)
          case registrationNoEither of
            Left err -> do
              logError $ "Error while decrypting document number: " <> (req.documentNumber & unEncrypted . encrypted) <> " with err: " <> show err
              pure Nothing
            Right registrationNo -> do
              rcNoEnc <- encrypt registrationNo
              RCQuery.findByCertificateNumberHash (rcNoEnc & hash)
        _ -> pure Nothing

      if req.status == "pending" || req.status == "source_down_retrying"
        then do
          msg <- toVerificationMessage VerificationInProgress language >>= addVerificationReasons language (mbRC <&> (.failedRules))
          return (PENDING, msg)
        else do
          message <- getMessageFromResponse language req.verificaitonResponse >>= addVerificationReasons language (mbRC <&> (.failedRules))
          return (FAILED, message)
    Nothing -> do
      if imagesNum > onboardingTryLimit * bool 1 2 (docType == DVC.DriverLicense)
        then do
          msg <- toVerificationMessage LimitExceed language
          return (LIMIT_EXCEED, msg)
        else do
          msg <- toVerificationMessage NoDcoumentFound language
          return (NO_DOC_AVAILABLE, msg)

getMessageFromResponse :: OnboardingFlow m r => Language -> Maybe Text -> m Text
getMessageFromResponse language response = do
  case response of
    Just res
      | "id_not_found" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | "source_down" `T.isInfixOf` res -> toVerificationMessage VerificationInProgress language
      | "TIMEOUT" `T.isInfixOf` res -> toVerificationMessage VerficationFailed language
      | "BAD_REQUEST" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | "422" `T.isInfixOf` res -> toVerificationMessage InvalidDocumentNumber language
      | otherwise -> toVerificationMessage Other language
    Nothing -> toVerificationMessage Other language

mkCommonDocumentItem :: DCDOD.CommonDriverOnboardingDocuments -> CommonDocumentItem
mkCommonDocumentItem doc =
  CommonDocumentItem
    { documentType = doc.documentType,
      documentData = renderCommonDocumentData doc.documentData,
      verificationStatus = mapVerificationStatus doc.verificationStatus,
      rejectReason = doc.rejectReason,
      documentImageId = getId <$> doc.documentImageId,
      createdAt = doc.createdAt,
      updatedAt = doc.updatedAt
    }
  where
    mapVerificationStatus :: Documents.VerificationStatus -> ResponseStatus
    mapVerificationStatus Documents.PENDING = PENDING
    mapVerificationStatus Documents.VALID = VALID
    mapVerificationStatus Documents.INVALID = INVALID
    mapVerificationStatus Documents.MANUAL_VERIFICATION_REQUIRED = MANUAL_VERIFICATION_REQUIRED
    mapVerificationStatus _ = PENDING -- default case

getDigilockerResponseCode :: OnboardingFlow m r => Id DP.Person -> m (Maybe Text)
getDigilockerResponseCode driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ mbSession >>= (.responseCode)

getRecentFleetDriverAssociationInfo :: OnboardingFlow m r => Id DP.Person -> m (Maybe DOVT.FleetInfo)
getRecentFleetDriverAssociationInfo driverId = do
  fdas <- QFDA.findAllByDriverIdWithStatus driverId
  case listToMaybe (sortOn (Down . (.createdAt)) fdas) of
    Nothing -> pure Nothing
    Just fda -> do
      mbFleetPerson <- QPerson.findById (Id fda.fleetOwnerId)
      case mbFleetPerson of
        Nothing -> pure Nothing
        Just fleetPerson -> do
          fleetPhoneNumber <- decrypt `mapM` fleetPerson.mobileNumber
          fleetOwnerInfo <- QFOI.findByPrimaryKey (Id fda.fleetOwnerId)
          now <- getCurrentTime
          pure $
            Just
              DOVT.FleetInfo
                { id = fda.fleetOwnerId,
                  ownerName = fleetPerson.firstName <> maybe "" (" " <>) fleetPerson.lastName,
                  fleetName = fleetOwnerInfo >>= (.fleetName),
                  phoneNumber = fleetPhoneNumber,
                  address = fleetOwnerInfo >>= (.stripeAddress),
                  requestReason = fda.requestReason,
                  responseReason = fda.responseReason,
                  chargesEnabled = Nothing,
                  createdAt = fda.createdAt,
                  isActive = fda.isActive,
                  isAssociated = maybe False (> now) fda.associatedTill,
                  associatedTill = fda.associatedTill
                }

getDigilockerDocStatusMap :: OnboardingFlow m r => Id DP.Person -> m DocStatus.DocStatusMap
getDigilockerDocStatusMap driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ maybe DocStatus.emptyDocStatusMap (.docStatus) mbSession

mapDigilockerToResponseStatus :: DocStatus.DocStatusEnum -> Maybe ResponseStatus
mapDigilockerToResponseStatus DocStatus.DOC_PENDING = Just PENDING
mapDigilockerToResponseStatus DocStatus.DOC_FAILED = Just FAILED
mapDigilockerToResponseStatus DocStatus.DOC_CONSENT_DENIED = Just CONSENT_DENIED
mapDigilockerToResponseStatus DocStatus.DOC_PULL_REQUIRED = Just PULL_REQUIRED
mapDigilockerToResponseStatus DocStatus.DOC_SUCCESS = Just VALID
