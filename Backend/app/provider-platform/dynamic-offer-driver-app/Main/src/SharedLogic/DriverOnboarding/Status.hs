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
    recomputeFleetVerifiedAndEnabled,
    recomputeDriverVerifiedAndEnabled,
    refreshDocsVerificationStatusesWithStatus,
    botApproveAndReconcile,
    forkRecomputeVehicleVerified,
    activateRCAutomatically,
    mkCommonDocumentItem,
    checkInspectionHubRequestCreated,
    getInspectionHubStatusAndReason,
    checkLMSTrainingStatus,
    StatusEvent (..),
    processStatusEvent,
    processStatusEventForReminder,
    markDocsVerificationStatusRejectedForPerson,
    markDocsVerificationStatusRejectedForRC,
    ensureNoActiveRidesUnderFleet,
    cascadeFleetDisableToDrivers,
    cascadeFleetEnableToDrivers,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Extra (anyM)
import Data.Either (fromRight)
import Data.List (nub)
import qualified Data.Text as T
import qualified Domain.Action.UI.DriverOnboarding.DriverLicense as DDL
import qualified Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate as DomainRC
import qualified Domain.Types.AadhaarCard as DAadhaarCard
import qualified Domain.Types.CommonDriverOnboardingDocuments as DCDOD
import qualified Domain.Types.DocStatus as DocStatus
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DocumentVerificationConfig as DDVC
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverLicense as DL
import Domain.Types.Extra.IdfyVerification (docTypeToText)
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig as FODVC
import qualified Domain.Types.FleetOwnerInformation as DFOI
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.OperationHubRequests as DOHR
import qualified Domain.Types.Person as DP
import qualified Domain.Types.TransporterConfig as DTC
import qualified Domain.Types.VehicleCategory as DVC
import qualified Domain.Types.VehicleRegistrationCertificate as RC
import Environment
import GHC.Records.Extra (HasField)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Types (Language, ServiceFlow)
import qualified Kernel.External.Verification as KEV
import Kernel.Prelude hiding (HasField)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error hiding (Unauthorized)
import Kernel.Types.Id
import Kernel.Utils.Common hiding (HasField)
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Digilocker as SDDigilocker
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Beam.IssueManagement ()
import qualified Storage.Cac.TransporterConfig as SCTC
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
import qualified Storage.Queries.DriverGstin as QDGST
import qualified Storage.Queries.DriverIdentityInfo as QDII
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverInformationExtra as DIQueryExtra
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
import qualified Tools.SMS as Sms
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

data StatusEvent
  = PersonDocChangedEvent (Id DP.Person)
  | VehicleDocChangedEvent (Id RC.VehicleRegistrationCertificate)
  | DocumentUnlinkedEvent (Id DP.Person)
  | PersonDocumentExpiredEvent (Id DP.Person)
  | RCDocumentExpiredEvent (Id RC.VehicleRegistrationCertificate)
  | PersonReminderProcessedEvent (Id DP.Person)
  | RCReminderProcessedEvent (Id RC.VehicleRegistrationCertificate)

-- PENDING means "pending verification"
-- FAILED is used when verification is failed
-- UNAUTHORIZED is used when a driver is not eligible to be onboarded to the platform
-- INVALID is the state
--   which the doc switches to when, for example, it's expired or when it is invalidated from dashboard.
-- PULL_REQUIRED is used when a document needs to be pulled from DigiLocker
-- CONSENT_DENIED is used when user denies consent for DigiLocker verification

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

-- | True iff the driver has an active fleet association (→ treated as fleet-side for the applicableTo split).
hasActiveFleetAssociation :: Id DP.Person -> Flow Bool
hasActiveFleetAssociation personId = isJust <$> QFDA.findByDriverId personId True

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

-- | Pick the fleet config row for @docType@. Only for a fleet role (FLEET_OWNER/FLEET_BUSINESS) is the
--   role filter applied — prefer the row matching that exact role, falling back to the first @docType@ row.
--   For any other role, behave like the old code: just take the first @docType@ row (role-blind).
findFleetConfigForRole :: DVC.DocumentType -> DP.Role -> [FODVC.FleetOwnerDocumentVerificationConfig] -> Maybe FODVC.FleetOwnerDocumentVerificationConfig
findFleetConfigForRole docType role fleetConfigs
  | isFleetRole role =
    find (\c -> c.documentType == docType && c.role == role) fleetConfigs
      <|> find (\c -> c.documentType == docType) fleetConfigs
  | otherwise = find (\c -> c.documentType == docType) fleetConfigs

-- | Non-throwing: fetch fleet configs for @docType@ via config-pilot (in-mem cached) and pick the row for
--   @role@ (see 'findFleetConfigForRole'). Returns Nothing when the city has no such config.
findFleetDocVerificationConfig :: Id DMOC.MerchantOperatingCity -> DVC.DocumentType -> DP.Role -> Flow (Maybe FODVC.FleetOwnerDocumentVerificationConfig)
findFleetDocVerificationConfig merchantOpCityId docType role = do
  configs <-
    getConfig
      (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Just docType, role = Just role})
      (Just (filter (\c -> c.documentType == docType) <$> CQFODVC.findAllByMerchantOpCityId merchantOpCityId Nothing))
  pure $ findFleetConfigForRole docType role configs

-- | Throwing variant of 'findFleetDocVerificationConfig' — errors if the city has no such config.
getFleetDocVerificationConfig :: Id DMOC.MerchantOperatingCity -> DVC.DocumentType -> DP.Role -> Flow FODVC.FleetOwnerDocumentVerificationConfig
getFleetDocVerificationConfig merchantOpCityId docType role =
  findFleetDocVerificationConfig merchantOpCityId docType role
    >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOpCityId.getId (show docType))

data StatusRes' = StatusRes'
  { driverDocuments :: [DocumentStatusItem],
    vehicleDocuments :: [VehicleDocumentItem],
    enabled :: Bool,
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
  RC.VehicleRegistrationCertificate ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Maybe Bool ->
  Flow (VehicleDocumentItem, [DVC.DocumentVerificationConfig])
fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig language reqRegistrationNo onlyMandatoryDocs = do
  let entity = IQuery.VehicleRCEntity rc
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
  allDocumentVerificationConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing))
  let skipMessages = True -- Skip translations, only need status check for inspection
  vehicleDocumentsUnverified <- fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs skipMessages
  vehicleDoc <-
    find (\doc -> doc.registrationNo == reqRegistrationNo) vehicleDocumentsUnverified
      & fromMaybeM (InvalidRequest $ "Vehicle doc not found for vehicle with registartionNo " <> reqRegistrationNo)
  pure (vehicleDoc, allDocumentVerificationConfigs)

-- | Fetch this RC's vehicle docs and check all enabling docs are VALID (non-BOT path; matches main's ForEnabling).
fetchAndCheckVehicleDocsValidForEnabling ::
  RC.VehicleRegistrationCertificate ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Flow Bool
fetchAndCheckVehicleDocsValidForEnabling rc merchantOperatingCity transporterConfig language reqRegistrationNo = do
  (vehicleDoc, allDocumentVerificationConfigs) <- fetchVehicleDocStatusesForRC rc merchantOperatingCity transporterConfig language reqRegistrationNo (Just True)
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
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow Bool
fetchAndCheckDriverDocsValidForEnabling person merchantOperatingCity transporterConfig language = do
  (allDocVerificationConfigs, driverDocuments, vehicleCategory) <- fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language (Just True)
  pure $ checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory Nothing

-- | All mandatory driver docs VALID, over already-fetched statuses (no fetch); applies the fleet filter.
checkAllDriverDocsValidForFetchedDocs ::
  DP.Person ->
  DTC.TransporterConfig ->
  DocVerificationConfigs ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Flow Bool
checkAllDriverDocsValidForFetchedDocs person transporterConfig allDocVerificationConfigs driverDocuments vehicleCategory = do
  mbIsFleetDriver <-
    if transporterConfig.enableBotFlow == Just True
      then Just . isJust <$> QFDA.findByDriverId person.id True
      else pure Nothing
  pure $ checkAllDriverDocsValid' ForVerified mbIsFleetDriver allDocVerificationConfigs person.role driverDocuments vehicleCategory Nothing

-- | Fetch a person's driver docs with their per-doc statuses, plus the configs and the active vehicle
--   category. @onlyMandatoryDocs = Just True@ fetches the mandatory set; @Nothing@ fetches the full
--   default list (needed e.g. for the dependency check). Extracted from fetchAndCheckDriverDocsValidForEnabling
--   so callers that need the per-doc statuses (not just the Bool) can reuse the same fetch.
fetchDriverDocStatusesForPerson ::
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Maybe Bool ->
  Flow (DocVerificationConfigs, [DocumentStatusItem], DVC.VehicleCategory)
fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language onlyMandatoryDocs = do
  let useHVSdkForDL = Just True
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
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
  pure (allDocVerificationConfigs, driverDocuments, vehicleCategory)

refreshVehicleDocsVerificationStatusForRC ::
  Maybe DTC.TransporterConfig ->
  Id RC.VehicleRegistrationCertificate ->
  Flow ()
refreshVehicleDocsVerificationStatusForRC mbTransporterConfig rcId = do
  rc <- RCQuery.findById rcId >>= fromMaybeM (InternalError $ "RC not found by id " <> rcId.getId)
  merchantOpCityId <- rc.merchantOperatingCityId & fromMaybeM (InternalError $ "merchantOperatingCityId missing for RC " <> rc.id.getId)
  transporterConfig <-
    maybe
      (getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId))
      pure
      mbTransporterConfig
  when (transporterConfig.enableManualDocumentStatusCheck == Just True) $ do
    merchantOperatingCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
    registrationNo <- decrypt rc.certificateNumber
    let entity = IQuery.VehicleRCEntity rc
    entityImages <- IQuery.findAllByEntityId transporterConfig entity
    now <- getCurrentTime
    let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
        language = merchantOperatingCity.language
        onlyMandatoryDocs = Just True
        skipMessages = True
    allDocumentVerificationConfigs <- getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCity.id.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing))
    vehicleDocuments <- fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just registrationNo) onlyMandatoryDocs skipMessages
    let newVehicleStatus =
          Just $
            case find (\vehicleDoc -> vehicleDoc.registrationNo == registrationNo) vehicleDocuments of
              Just vehicleDoc -> computeAdminDocsVerificationStatus vehicleDoc.documents
              Nothing -> computeAdminDocsVerificationStatus []
    rcHash <- getDbHash registrationNo
    RCQuery.updateDocsVerificationStatusByCertificateNumberHash newVehicleStatus rcHash

processStatusEvent ::
  Maybe DP.Person ->
  Maybe DTC.TransporterConfig ->
  StatusEvent ->
  Flow (Maybe Bool)
processStatusEvent mbPerson mbTransporterConfig = \case
  PersonDocChangedEvent personId -> runPersonRefresh personId
  VehicleDocChangedEvent rcId -> runRCRefresh rcId
  DocumentUnlinkedEvent personId -> runPersonRefresh personId
  PersonDocumentExpiredEvent personId -> runPersonRefresh personId
  RCDocumentExpiredEvent rcId -> runRCRefresh rcId
  PersonReminderProcessedEvent personId -> runPersonRefresh personId
  RCReminderProcessedEvent rcId -> runRCRefresh rcId
  where
    lockTTLSeconds = 30
    mkPersonDocsStatusKey personId = "DocsStatus:Person:" <> personId.getId
    mkRCDocsStatusKey rcId = "DocsStatus:RC:" <> rcId.getId
    withPersonDocsStatusLock personId action =
      Hedis.withLockRedisAndReturnValue (mkPersonDocsStatusKey personId) lockTTLSeconds action
    withRCDocsStatusLock rcId action =
      Hedis.withLockRedisAndReturnValue (mkRCDocsStatusKey rcId) lockTTLSeconds action
    runPersonRefresh personId = do
      statusRes <- withPersonDocsStatusLock personId $ refreshDocsVerificationStatusesWithStatus mbPerson mbTransporterConfig personId
      pure $ Just statusRes.enabled
    runRCRefresh rcId = do
      withRCDocsStatusLock rcId $ refreshVehicleDocsVerificationStatusForRC mbTransporterConfig rcId
      pure Nothing

processStatusEventForReminder ::
  ( EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  StatusEvent ->
  m ()
processStatusEventForReminder = \case
  PersonReminderProcessedEvent personId -> markDocsVerificationStatusRejectedForPerson personId
  PersonDocumentExpiredEvent personId -> markDocsVerificationStatusRejectedForPerson personId
  RCReminderProcessedEvent rcId -> markDocsVerificationStatusRejectedForRC rcId
  RCDocumentExpiredEvent rcId -> markDocsVerificationStatusRejectedForRC rcId
  _ -> pure ()

markDocsVerificationStatusRejectedForPerson ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    EncFlow m r,
    MonadFlow m
  ) =>
  Id DP.Person ->
  m ()
markDocsVerificationStatusRejectedForPerson personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let targetStatus = Just DDVS.ADMIN_REJECTED
  if isFleetRole person.role
    then do
      fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      when (fleetOwnerInfo.docsVerificationStatus /= targetStatus) $
        QFOI.updateByPrimaryKey fleetOwnerInfo {DFOI.docsVerificationStatus = targetStatus}
    else do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
      when (driverInfo.docsVerificationStatus /= targetStatus) $
        DIQuery.updateByPrimaryKey driverInfo {DI.docsVerificationStatus = targetStatus}

markDocsVerificationStatusRejectedForRC ::
  ( EsqDBFlow m r,
    CacheFlow m r,
    MonadFlow m
  ) =>
  Id RC.VehicleRegistrationCertificate ->
  m ()
markDocsVerificationStatusRejectedForRC rcId = do
  rc <- RCQuery.findById rcId >>= fromMaybeM (InternalError $ "RC not found by id: " <> rcId.getId)
  let targetStatus = Just DDVS.ADMIN_REJECTED
  when (rc.docsVerificationStatus /= targetStatus) $
    RCQuery.updateByPrimaryKey rc {RC.docsVerificationStatus = targetStatus}

refreshDocsVerificationStatusesWithStatus ::
  Maybe DP.Person ->
  Maybe DTC.TransporterConfig ->
  Id DP.Person ->
  Flow StatusRes'
refreshDocsVerificationStatusesWithStatus mbPerson mbTransporterConfig personId = do
  PersonStatusContext {statusPerson, statusEntityImagesInfo} <- loadPersonStatusContext mbPerson mbTransporterConfig personId
  let onlyMandatoryDocs = Just True
      shouldActivateRc = False
      skipMessages = True
  statusHandler' statusPerson statusEntityImagesInfo Nothing Nothing Nothing Nothing (Just True) shouldActivateRc onlyMandatoryDocs skipMessages

loadPersonStatusContext ::
  Maybe DP.Person ->
  Maybe DTC.TransporterConfig ->
  Id DP.Person ->
  Flow PersonStatusContext
loadPersonStatusContext mbPerson mbTransporterConfig personId = do
  person <- maybe (runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)) pure mbPerson
  transporterConfig <-
    maybe
      (getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId))
      pure
      mbTransporterConfig
  merchantOperatingCity <- CQMOC.findById person.merchantOperatingCityId >>= fromMaybeM (MerchantOperatingCityNotFound person.merchantOperatingCityId.getId)
  let entity = IQuery.PersonEntity person
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let statusEntityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
  pure PersonStatusContext {statusPerson = person, statusEntityImagesInfo}

buildVehicleDocsContext ::
  DP.Person ->
  IQuery.EntityImagesInfo ->
  Language ->
  Maybe Bool ->
  Bool ->
  Maybe Text ->
  Flow VehicleDocsContext
buildVehicleDocsContext person entityImagesInfo language onlyMandatoryDocs skipMessages mbReqRegistrationNo = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
  allDocVerificationConfigs <-
    if isFleetRole person.role
      then Left <$> getConfig (FleetOwnerDocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, role = Nothing}) (Just (CQFODVC.findAllByMerchantOpCityId merchantOpCityId Nothing))
      else Right <$> getConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId, documentType = Nothing, vehicleCategory = Nothing}) (Just (CQDVC.findAllByMerchantOpCityId merchantOpCityId Nothing))
  let driverDocConfigs = fromRight [] allDocVerificationConfigs :: [DVC.DocumentVerificationConfig]
  vehicleDocumentsUnverified <-
    if isFleetRole person.role
      then pure []
      else fetchVehicleDocuments entityImagesInfo driverDocConfigs language mbReqRegistrationNo onlyMandatoryDocs skipMessages
  pure VehicleDocsContext {allDocVerificationConfigs, driverDocConfigs, vehicleDocumentsUnverified}

-- | The onboarding status engine. Builds the per-document status list returned to the app, and as a
--   side-effect mutates onboarding state:
--     • enableBotFlow on  — recompute* are the source of truth for verified/enabled (both directions);
--       `approved` is BOT-owned; RC auto-activation and legacy inline auto-enable are suppressed.
--     • enableBotFlow off — legacy: auto-enable/RC-activate/fleet-disable inline (unchanged).
statusHandler' ::
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
  Flow StatusRes'
statusHandler' person entityImagesInfo makeSelfieAadhaarPanMandatory prefillData onboardingVehicleCategory mDL useHVSdkForDL shouldActivateRc onlyMandatoryDocs skipMessages = do
  let merchantId = entityImagesInfo.merchantOperatingCity.merchantId
      merchantOperatingCity = entityImagesInfo.merchantOperatingCity
      merchantOpCityId = merchantOperatingCity.id
      transporterConfig = entityImagesInfo.transporterConfig
      personId = person.id
  let language = fromMaybe merchantOperatingCity.language person.language

  VehicleDocsContext {allDocVerificationConfigs, driverDocConfigs, vehicleDocumentsUnverified} <-
    buildVehicleDocsContext person entityImagesInfo language onlyMandatoryDocs skipMessages Nothing

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

  let enableBotFlow = transporterConfig.enableBotFlow == Just True

  vehicleDocuments <-
    if enableBotFlow
      then do
        -- BOT flow: verified/enabled are purely doc-driven (see recomputeDriverVerifiedAndEnabled / recomputeFleetVerifiedAndEnabled),
        -- independent of separateDriverVehicleEnablement. The BOT sets `approved`; statusHandler derives the rest.
        let vehicleCategory = fromMaybe DVC.CAR $ onboardingVehicleCategory <|> listToMaybe possibleVehicleCategories
        when (isFleetRole person.role) $
          void $ recomputeFleetVerifiedAndEnabled person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
        when (person.role == DP.DRIVER) $
          void $ recomputeDriverVerifiedAndEnabled merchantOpCityId merchantId person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory (mDL >>= (.driverName)) onboardingVehicleCategory transporterConfig Nothing
        -- Vehicle status list (+ vehicle `verified` write, handled inside getVehicleDocuments under enableBotFlow)
        getVehicleDocuments driverDocConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification True driverDocuments merchantOpCityId
      else -- Legacy enablement (unchanged): conditional on separateDriverVehicleEnablement.

        if isFleetRole person.role || transporterConfig.separateDriverVehicleEnablement == Just True
          then do
            -- Fleet owner enablement/disablement (uses FleetOwnerInformation)
            when (isFleetRole person.role) $ do
              let vehicleCategory = DVC.CAR
                  allFleetDocsVerified = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
                  isRejectedMandatoryFleetDoc doc =
                    doc.verificationStatus `elem` [FAILED, INVALID]
                      && not (checkIfDocumentValid allDocVerificationConfigs person.role doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory)
              -- First check if fleet should be disabled (has rejected mandatory docs)
              when (any isRejectedMandatoryFleetDoc driverDocuments && transporterConfig.allowDisableFleetOnRejectionDoc == Just True) $
                disableFleetOwnerOnRejectionDoc personId
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

  when (transporterConfig.enableManualDocumentStatusCheck == Just True) $ do
    (driverDocsForPersist, vehicleDocsForPersist) <-
      if onlyMandatoryDocs == Just True
        then pure (driverDocuments, vehicleDocuments)
        else do
          mandatoryDriverDocTypes <-
            getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories person.role (Just True) (transporterConfig.enableBotFlow == Just True)
          let driverDocumentsMandatory =
                filter (\d -> d.documentType `elem` mandatoryDriverDocTypes) driverDocuments
          vehicleDocumentsMandatory <- forM vehicleDocuments $ \v -> do
            mandatoryTypesForVehicle <-
              getVehicleDocTypes merchantOpCityId driverDocConfigs v.verifiedVehicleCategory v.userSelectedVehicleCategory (Just True)
            pure $ v {documents = filter (\d -> d.documentType `elem` mandatoryTypesForVehicle) v.documents}
          pure (driverDocumentsMandatory, vehicleDocumentsMandatory)
    let vehicleDocsForPersist' =
          if transporterConfig.separateDriverVehicleEnablement == Just True && person.role == DP.DRIVER
            then []
            else vehicleDocsForPersist
    persistDocsVerificationStatuses person driverDocsForPersist vehicleDocsForPersist'

  enabled <-
    if isFleetRole person.role
      then do
        fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
        return fleetOwnerInfo.enabled
      else do
        driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
        return driverInfo.enabled

  digilockerResponseCode <- getDigilockerResponseCode personId

  digilockerAuthorizationUrl <-
    if transporterConfig.digilockerEnabled == Just True
      then SDDigilocker.getDigiLockerAuthorizationUrl personId
      else pure Nothing

  return $
    StatusRes'
      { driverDocuments,
        vehicleDocuments,
        enabled = enabled,
        manualVerificationRequired = transporterConfig.requiresOnboardingInspection,
        driverLicenseDetails = dlDetails,
        vehicleRegistrationCertificateDetails = rcDetails,
        digilockerResponseCode = digilockerResponseCode,
        digilockerAuthorizationUrl = digilockerAuthorizationUrl
      }
  where
    getVehicleDocuments driverDocConfs role vehicleDocumentsUnverified requiresOnboardingInspection vehicleCategoryExcludedFromVerification separateEnablement driverDocuments merchantOpCityId = do
      let personId = person.id
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
        -- isActive=False means RC was explicitly deactivated — skip auto-reactivation
        -- Under enableBotFlow: write VRC.verified (= all isMandatory vehicle docs VALID) both ways;
        -- `approved` and RC activation are BOT-owned (suppressed below).
        let enableBotFlow = entityImagesInfo.transporterConfig.enableBotFlow == Just True
        when enableBotFlow $ do
          let allVehicleMandatoryDocsValid = checkAllVehicleDocsValidForVerified driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory
          rcHash <- getDbHash vehicleDoc.registrationNo
          RCQuery.updateVerifiedByCertificateNumberHash (Just allVehicleMandatoryDocsValid) rcHash
        mbVehicle <- QVehicle.findById personId
        when (shouldActivateRc && isNothing mbVehicle && checkToActivateRC && role == DP.DRIVER && not enableBotFlow && (isActive || not (fromMaybe False entityImagesInfo.transporterConfig.dontAutoEnableDriver))) $ do
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

isFleetRole :: DP.Role -> Bool
isFleetRole DP.FLEET_OWNER = True
isFleetRole DP.FLEET_BUSINESS = True
isFleetRole _ = False

fetchDriverDocuments ::
  IQuery.EntityImagesInfo ->
  DocVerificationConfigs ->
  [DVC.VehicleCategory] ->
  DP.Person ->
  Language ->
  Maybe Bool ->
  Maybe Bool ->
  Bool ->
  Flow [DocumentStatusItem]
fetchDriverDocuments entityImagesInfo allDocVerificationConfigs possibleVehicleCategories person language useHVSdkForDL onlyMandatoryDocs skipMessages = do
  let role = person.role
      merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      driverId = person.id
      transporterConfig = entityImagesInfo.transporterConfig
      isDigiLockerEnabled = fromMaybe False transporterConfig.digilockerEnabled
      enableMetadata = fromMaybe False transporterConfig.enableDocumentMetadata

  digilockerDocStatusMap <- if isDigiLockerEnabled then getDigilockerDocStatusMap driverId else pure DocStatus.emptyDocStatusMap

  driverDocumentTypes <- getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs (transporterConfig.enableBotFlow == Just True)
  driverDocumentTypes `forM` \docType -> do
    let mbDocStatus = if isDigiLockerEnabled then DocStatus.getDocStatus docType digilockerDocStatusMap else Nothing
        responseCode = mbDocStatus >>= (.responseCode)
        mbDocVerificationStatus = mbDocStatus >>= (mapDigilockerToResponseStatus . (.status))

    (mbProcessedStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2, mbMetadata) <- getProcessedDriverDocuments person.role person.id entityImagesInfo docType useHVSdkForDL enableMetadata
    (status, mbReason, mbUrl, mbExpiryFinal, mbS3PathFinal, mbImageIdFinal, mbImageId2Final) <- case mbProcessedStatus of
      Just VALID -> pure (VALID, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2)
      Just s -> pure (s, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2)
      Nothing -> case mbDocVerificationStatus of
        Just docStatus -> pure (docStatus, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
        Nothing -> getInProgressDriverDocuments person.role driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs

    mbMessage <- documentStatusMessage status mbReason docType mbUrl language skipMessages
    let finalMessage = mbReason <|> (if isDigiLockerEnabled then responseCode else Nothing) <|> mbMessage
    return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = finalMessage, verificationUrl = mbUrl, s3Path = mbS3PathFinal, imageId = mbImageIdFinal, imageId2 = mbImageId2Final, documentExpiry = mbExpiryFinal, metadata = mbMetadata}

getDriverDocTypes ::
  Id DMOC.MerchantOperatingCity ->
  DocVerificationConfigs ->
  [DVC.VehicleCategory] ->
  DP.Role ->
  Maybe Bool ->
  Bool ->
  Flow [DVC.DocumentType]
getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs enableBotFlow = do
  case allDocVerificationConfigs of
    Left fleetConfigs -> do
      -- Fleet-role drift defense: person.role and fleet_owner_information.fleet_type
      -- can drift apart (e.g. role=FLEET_OWNER but fleet_type=BUSINESS_FLEET, so
      -- configs are seeded for FLEET_BUSINESS). For any fleet role, if no configs
      -- match the exact role, fall back to configs for any fleet role in the city.
      let exactRoleConfigs = filter (\config -> config.role == role) fleetConfigs
          anyFleetRoleConfigs = filter (\config -> isFleetRole config.role) fleetConfigs
          effectiveConfigs =
            if isFleetRole role && null exactRoleConfigs
              then anyFleetRoleConfigs
              else exactRoleConfigs
          -- BOT broadens the mandatory-fetch set to isMandatoryForEnabling so enabling-only docs
          -- (e.g. fleet OperatorPartnerCode) are fetched for the fleet `enabled` computation; non-BOT
          -- keeps the isMandatory-only set (backward compatible with main).
          mandatoryDocTypes = nub $ map (.documentType) $ filter (\c -> if enableBotFlow then fromMaybe c.isMandatory c.isMandatoryForEnabling else c.isMandatory) effectiveConfigs
          allRoleDocTypes = nub $ map (.documentType) effectiveConfigs
      when (isFleetRole role && null exactRoleConfigs && not (null anyFleetRoleConfigs)) $
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
      let isDriverSideDoc config = case config.documentCategory of
            Just DDVC.Driver -> True
            Just DDVC.Training -> True
            Just DDVC.Vehicle -> False
            Just DDVC.Permission -> False
            Nothing -> config.documentType `elem` SDO.defaultDriverDocumentTypes
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
          pure driverDocumentTypes
        else pure $ if null allDriverDocumentTypes then SDO.defaultDriverDocumentTypes else allDriverDocumentTypes

-- | All vehicle docs in the enabling set (isMandatoryForEnabling) VALID. Drives @enabled@ / RC activation.
checkAllVehicleDocsValidForEnabling ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValidForEnabling = checkAllVehicleDocsValid' ForEnabling

-- | Are all isMandatory vehicle docs VALID? Excludes isMandatoryForEnabling-only docs. Drives
--   @verified@. (Sibling checkAllVehicleDocsValidForEnabling uses the isMandatoryForEnabling set → @enabled@.)
checkAllVehicleDocsValidForVerified ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValidForVerified = checkAllVehicleDocsValid' ForVerified

checkAllVehicleDocsValid' ::
  MandatoryMode ->
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsValid' mode driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory = do
  -- Vehicle docs are not subject to the fleet-driver/DCO applicableTo split → Nothing.
  all (\doc -> checkIfDocumentValid' mode Nothing (Right driverDocConfs) DP.DRIVER doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus makeSelfieAadhaarPanMandatory) vehicleDoc.documents

-- | All driver docs in the enabling set (isMandatoryForEnabling) VALID; no applicableTo filtering. Drives @enabled@.
checkAllDriverDocsValidForEnabling ::
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsValidForEnabling = checkAllDriverDocsValid' ForEnabling Nothing

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

checkAllDriverDocsValid' ::
  MandatoryMode ->
  Maybe Bool ->
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsValid' mode mbIsFleetDriver allDocVerificationConfigs role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory = do
  all (\doc -> checkIfDocumentValid' mode mbIsFleetDriver allDocVerificationConfigs role doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments

-- | Driver (DCO + fleet driver): recompute verified/enabled from doc validity, both directions.
--     verified = all isMandatory docs VALID
--     enabled  = verified AND all isMandatoryForEnabling docs VALID AND approved == Just True
--   enabled routes through enableDriver (preserves alerts/SMS/LTS); disable via disableDriverWithAnalytics.
recomputeDriverVerifiedAndEnabled ::
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
  Flow Bool
recomputeDriverVerifiedAndEnabled merchantOpCityId merchantId person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory driverName onboardingVehicleCategory transporterConfig mbIsFleetDriver = do
  driverInfo <- DIQuery.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
  isFleetDriver <- maybe (hasActiveFleetAssociation person.id) pure mbIsFleetDriver
  -- A fleet driver (active fleet association) skips INDIVIDUAL-only docs like OperatorPartnerCode, via applicableTo.
  let allMandatoryDocsValid = checkAllDriverDocsValid' ForVerified (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      allEnablingDocsValid = checkAllDriverDocsValid' ForEnabling (Just isFleetDriver) allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
  when (allMandatoryDocsValid /= driverInfo.verified) $
    -- Downgrading verified revokes approved (re-approval required); the disable branch below does the same.
    DIQueryExtra.updateVerifiedAndApprovedState (cast person.id) allMandatoryDocsValid (if allMandatoryDocsValid then Nothing else Just False)
  let shouldEnable = allMandatoryDocsValid && allEnablingDocsValid && driverInfo.approved == Just True
  if shouldEnable && not driverInfo.enabled
    then do
      enableDriver merchantOpCityId person.id person.role driverName transporterConfig merchantId allMandatoryDocsValid
      whenJust onboardingVehicleCategory $ \category ->
        DIIQuery.updateOnboardingVehicleCategory (Just category) person.id
    else
      when (not shouldEnable && driverInfo.enabled) $
        SDO.disableDriverWithAnalytics merchantOpCityId person.id Nothing
  -- Return the freshly computed enable state so callers don't re-read driver_information (which could
  -- be stale under read-replica lag right after the write).
  pure shouldEnable

-- | Fleet owner (not fleet drivers — they go through recomputeDriverVerifiedAndEnabled). Recompute
--   verified/enabled from doc validity, both directions.
--     verified = all isMandatory fleet docs VALID            (excludes OperatorPartnerCode)
--     enabled  = all isMandatoryForEnabling fleet docs VALID (incl. OperatorPartnerCode — the BOT-set enable gate)
--   No `approved` flag and, under enableBotFlow, NO driver cascade — flags written directly.
recomputeFleetVerifiedAndEnabled ::
  DP.Person ->
  DocVerificationConfigs ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Flow Bool
recomputeFleetVerifiedAndEnabled person allDocVerificationConfigs driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory = do
  fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
  let allFleetMandatoryDocsValid = checkAllDriverDocsValidForVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
      allFleetEnablingDocsValid = checkAllDriverDocsValidForEnabling allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
  when (allFleetMandatoryDocsValid /= fleetOwnerInfo.verified) $
    QFOI.updateFleetOwnerVerifiedStatus allFleetMandatoryDocsValid person.id
  when (allFleetEnablingDocsValid /= fleetOwnerInfo.enabled) $
    QFOI.updateFleetOwnerEnabledStatus allFleetEnablingDocsValid person.id
  -- Return the freshly computed `enabled` so callers don't re-read FOI (which could be stale under
  -- read-replica lag right after the write).
  pure allFleetEnablingDocsValid

-- | BOT approve: throws (naming the offending docs) if any BotApproval dependency doc is NOT VALID;
--   otherwise forks the verified/enabled recompute. Throwing before the fork guarantees we never
--   force-enable a fleet/driver whose deps haven't passed.
botApproveAndReconcile ::
  DMOC.MerchantOperatingCity ->
  DP.Person ->
  DTC.TransporterConfig ->
  Flow ()
botApproveAndReconcile merchantOperatingCity person transporterConfig = do
  let language = fromMaybe merchantOperatingCity.language person.language
  (allDocVerificationConfigs, driverDocuments, vehicleCategory) <- fetchDriverDocStatusesForPerson person merchantOperatingCity transporterConfig language (Just True)
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
  -- Force BotApproval VALID: ReviewRequest isn't COMPLETED yet, but approval is committed.
  fork "botApproveAndReconcile: recompute verified/enabled" $ do
    let docs' = map forceBotApprovalValid driverDocuments
    if isFleetRole person.role
      then void $ recomputeFleetVerifiedAndEnabled person allDocVerificationConfigs docs' vehicleCategory Nothing
      else void $ recomputeDriverVerifiedAndEnabled merchantOperatingCity.id merchantOperatingCity.merchantId person allDocVerificationConfigs docs' vehicleCategory Nothing Nothing Nothing transporterConfig (Just isFleetDriver)
  where
    forceBotApprovalValid :: DocumentStatusItem -> DocumentStatusItem
    forceBotApprovalValid d
      | d.documentType == DVC.BotApproval = d {verificationStatus = VALID}
      | otherwise = d

-- | Fork RC-verified recompute (reusing fetched docs): RC.verified = all mandatory vehicle docs VALID,
--   with InspectionHub forced VALID (OHR not APPROVED yet). `approved`/RC activation stay BOT-owned.
forkRecomputeVehicleVerified ::
  Text ->
  VehicleDocumentItem ->
  [DVC.DocumentVerificationConfig] ->
  Flow ()
forkRecomputeVehicleVerified registrationNo vehicleDocItem allDocumentVerificationConfigs =
  fork "forkRecomputeVehicleVerified: recompute vehicle verified" $ do
    let vehicleDocItem' = vehicleDocItem {documents = map overrideInspectionHubAsValid vehicleDocItem.documents}
        allVehicleMandatoryDocsValid = checkAllVehicleDocsValidForVerified allDocumentVerificationConfigs vehicleDocItem' Nothing
    rcHash <- getDbHash registrationNo
    RCQuery.updateVerifiedByCertificateNumberHash (Just allVehicleMandatoryDocsValid) rcHash
  where
    overrideInspectionHubAsValid :: DocumentStatusItem -> DocumentStatusItem
    overrideInspectionHubAsValid d
      | d.documentType == DVC.InspectionHub = d {verificationStatus = VALID}
      | otherwise = d

-- | Enable a driver/fleet (cascades fleet→drivers). @verifiedToSet@ is written for `verified` too;
--   legacy callers pass True.
enableDriver :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> DP.Role -> Maybe Text -> DTC.TransporterConfig -> Id DM.Merchant -> Bool -> Flow ()
enableDriver merchantOpCityId personId role driverName transporterConfig merchantId verifiedToSet = do
  if isFleetRole role
    then do
      fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      unless (fleetOwnerInfo.enabled && fleetOwnerInfo.verified) $ do
        QFOI.updateFleetOwnerEnabledAndVerifiedStatus True verifiedToSet personId
        cascadeFleetEnableToDrivers personId
    else do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
      unless (driverInfo.enabled && driverInfo.verified) $ do
        SDO.enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId verifiedToSet
        whenJust driverName $ \name -> QPerson.updateName name personId
        sendEnablementSms merchantOpCityId personId transporterConfig merchantId

disableFleetOwnerOnRejectionDoc :: Id DP.Person -> Flow ()
disableFleetOwnerOnRejectionDoc personId = do
  fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
  when fleetOwnerInfo.enabled $ do
    ensureNoActiveRidesUnderFleet personId
    QFOI.updateFleetOwnerEnabledStatus False personId
    cascadeFleetDisableToDrivers personId

-- | Throw InvalidRequest if any driver under the fleet has a NEW or
--   INPROGRESS ride. Used as a guard before flipping fleet enabled to false.
ensureNoActiveRidesUnderFleet :: Id DP.Person -> Flow ()
ensureNoActiveRidesUnderFleet fleetOwnerId = do
  driverIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId.getId
  anyActive <- anyM (fmap isJust . QRideExtra.getUpcomingOrActiveByDriverId) driverIds
  when anyActive $
    throwError $ InvalidRequest "Cannot disable fleet: one or more drivers have active rides"

-- | Disable every active driver under the fleet, tagging each with the
--   FleetDisabled flag so the inverse cascade can find them.
cascadeFleetDisableToDrivers :: Id DP.Person -> Flow ()
cascadeFleetDisableToDrivers fleetOwnerId = do
  driverIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId.getId
  forM_ driverIds $ \driverId -> DIQueryExtra.markDisabledForFleetCascade (cast driverId)

-- | Re-enable drivers previously disabled by 'cascadeFleetDisableToDrivers'.
--   Drivers blocked for other reasons are left alone.
cascadeFleetEnableToDrivers :: Id DP.Person -> Flow ()
cascadeFleetEnableToDrivers fleetOwnerId = do
  driverIds <- QFDA.getActiveDriverIdsByFleetOwnerId fleetOwnerId.getId
  forM_ driverIds $ \driverId -> DIQueryExtra.clearFleetCascadeAndEnable (cast driverId)

sendEnablementSms :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> DTC.TransporterConfig -> Id DM.Merchant -> Flow ()
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

persistDocsVerificationStatuses ::
  DP.Person ->
  [DocumentStatusItem] ->
  [VehicleDocumentItem] ->
  Flow ()
persistDocsVerificationStatuses person driverDocuments vehicleDocuments = do
  let mandatoryDriverDocuments = driverDocuments
      mandatoryVehicleDocuments = vehicleDocuments

  if isFleetRole person.role
    then do
      fleetOwnerInfo <- QFOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      let newStatus = Just $ computeAdminDocsVerificationStatus mandatoryDriverDocuments
          docSummary =
            T.intercalate
              ", "
              (map (\d -> show d.documentType <> ":" <> show d.verificationStatus) mandatoryDriverDocuments)
      logInfo $
        "persistDocsVerificationStatuses(fleet): personId="
          <> person.id.getId
          <> ", oldStatus="
          <> show fleetOwnerInfo.docsVerificationStatus
          <> ", newStatus="
          <> show newStatus
          <> ", mandatoryDocs="
          <> docSummary
      when (fleetOwnerInfo.docsVerificationStatus /= newStatus) $
        QFOI.updateByPrimaryKey fleetOwnerInfo {DFOI.docsVerificationStatus = newStatus}
      persistVehicleDocsVerificationStatuses person.id mandatoryVehicleDocuments "fleet"
    else do
      driverInfo <- DIQuery.findById (cast person.id) >>= fromMaybeM (PersonNotFound person.id.getId)
      let newDriverStatus = Just $ computeAdminDocsVerificationStatus mandatoryDriverDocuments
          driverDocSummary =
            T.intercalate
              ", "
              (map (\d -> show d.documentType <> ":" <> show d.verificationStatus) mandatoryDriverDocuments)
      logInfo $
        "persistDocsVerificationStatuses(driver): personId="
          <> person.id.getId
          <> ", oldStatus="
          <> show driverInfo.docsVerificationStatus
          <> ", newStatus="
          <> show newDriverStatus
          <> ", mandatoryDriverDocs="
          <> driverDocSummary
      when (driverInfo.docsVerificationStatus /= newDriverStatus) $
        DIQuery.updateByPrimaryKey driverInfo {DI.docsVerificationStatus = newDriverStatus}
      persistVehicleDocsVerificationStatuses person.id mandatoryVehicleDocuments "driver"

persistVehicleDocsVerificationStatuses :: Id DP.Person -> [VehicleDocumentItem] -> Text -> Flow ()
persistVehicleDocsVerificationStatuses personId mandatoryVehicleDocuments roleTag = do
  forM_ mandatoryVehicleDocuments $ \vehicleDoc -> do
    let vehicleDocSummary =
          T.intercalate
            ", "
            (map (\d -> show d.documentType <> ":" <> show d.verificationStatus) vehicleDoc.documents)
        newVehicleStatus =
          Just $ computeAdminDocsVerificationStatus vehicleDoc.documents
    rcHash <- getDbHash vehicleDoc.registrationNo
    logInfo $
      "persistDocsVerificationStatuses(vehicle:"
        <> roleTag
        <> "): personId="
        <> personId.getId
        <> ", registrationNo="
        <> vehicleDoc.registrationNo
        <> ", newStatus="
        <> show newVehicleStatus
        <> ", mandatoryVehicleDocs="
        <> vehicleDocSummary
    RCQuery.updateDocsVerificationStatusByCertificateNumberHash newVehicleStatus rcHash

activateRCAutomatically :: Id DP.Person -> DMOC.MerchantOperatingCity -> Text -> Flow ()
activateRCAutomatically personId merchantOpCity rcNumber = do
  let rcStatusReq =
        DomainRC.RCStatusReq
          { rcNo = rcNumber,
            isActivate = True
          }
  void $ DomainRC.linkRCStatus (personId, merchantOpCity.merchantId, merchantOpCity.id) False rcStatusReq

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
               MANUAL_VERIFICATION_REQUIRED -> config.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True
checkIfDocumentValid' mode mbIsFleetDriver (Right driverConfigs) _role docType category status makeSelfieAadhaarPanMandatory = do
  let mbVerificationConfig = find (\config -> config.documentType == docType && config.vehicleCategory == category) driverConfigs
  case mbVerificationConfig of
    Just verificationConfig ->
      -- A doc blocks only if it is mandatory under the mode AND applies to this driver's type.
      -- docAppliesToDriver Nothing _ = True (legacy), so legacy callers are unaffected.
      not (isDocRequiredFor mode verificationConfig && docAppliesToDriver mbIsFleetDriver verificationConfig.applicableTo && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory))
        || ( case status of
               MANUAL_VERIFICATION_REQUIRED -> verificationConfig.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True

getProcessedDriverDocuments :: DP.Role -> Id DP.Person -> IQuery.EntityImagesInfo -> DVC.DocumentType -> Maybe Bool -> Bool -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text, Maybe DocumentMetadata)
getProcessedDriverDocuments role driverId entityImagesInfo docType useHVSdkForDL enableMetadata = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      (mbS3Path, mbImageId) = getImageMetaFromLatestImage entityImagesInfo docType
      lookupImage imgId =
        let mbImg = find (\img -> img.id == imgId) entityImagesInfo.entityImages
            iid = Just imgId.getId
         in (mbImg <&> (.s3Path), iid)
      lookupImageFailReason imgId =
        extractImageFailReason (find (\img -> img.id == imgId) entityImagesInfo.entityImages >>= (.failureReason))
      commonDocStatus dt = do
        mbDoc <- listToMaybe <$> QCommonDocExtra.findLatestByDriverIdAndDocumentType (Just driverId) dt
        let (status, reason, url) = checkImageValidity entityImagesInfo dt
        case mbDoc of
          Just doc ->
            let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage doc.documentImageId
             in return (Just (mapStatus doc.verificationStatus), doc.rejectReason <|> reason, url, Nothing, s3, iid, Nothing, Nothing)
          Nothing -> return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
      mkDLMetadata mbDl =
        if enableMetadata
          then forM mbDl $ \dl -> do
            licenseNumberDec <- decrypt dl.licenseNumber
            pure $ DLMetadata DLDocumentMetadata {driverLicenseNumber = licenseNumberDec, driverDateOfBirth = dl.driverDob, dateOfExpiry = dl.licenseExpiry}
          else pure Nothing
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
          mbDlMetadata <- mkDLMetadata mbDL'
          return (mapStatus <$> (mbDL' <&> (.verificationStatus)), reason, Nothing, mbDL' <&> (.licenseExpiry), s3, iid, iid2, mbDlMetadata)
        else do
          let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbDL
              iid2 = mbDL >>= (.documentImageId2) <&> (.getId)
              reason = (mbDL >>= (.rejectReason)) <|> (if (mbDL <&> (.verificationStatus)) == Just Documents.INVALID then (mbDL <&> (.documentImageId1)) >>= lookupImageFailReason else Nothing)
          mbDlMetadata <- mkDLMetadata mbDL
          return (mapStatus <$> (mbDL <&> (.verificationStatus)), reason, Nothing, mbDL <&> (.licenseExpiry), s3, iid, iid2, mbDlMetadata)
    DVC.AadhaarCard -> do
      mbAadhaarCard <- QAadhaarCard.findByPrimaryKey driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage (mbAadhaarCard >>= (.aadhaarFrontImageId))
          iid2 = mbAadhaarCard >>= (.aadhaarBackImageId) <&> (.getId)
          reason = if (mbAadhaarCard <&> (.verificationStatus)) == Just Documents.INVALID then ((mbAadhaarCard >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbAadhaarCard >>= (.aadhaarFrontImageId)) >>= lookupImageFailReason) else Nothing
      mbAadhaarMetadata <-
        if enableMetadata
          then forM mbAadhaarCard $ \aadhaar -> do
            aadhaarNumberDec <- mapM decrypt aadhaar.aadhaarNumber
            pure $ AadhaarMetadata AadhaarDocumentMetadata {aadhaarNumber = aadhaarNumberDec, nameOnCard = aadhaar.nameOnCard, dateOfBirth = aadhaar.dateOfBirth, address = aadhaar.address}
          else pure Nothing
      return (mapStatus . (.verificationStatus) <$> mbAadhaarCard, reason, Nothing, Nothing, s3, iid, iid2, mbAadhaarMetadata)
    DVC.Permissions -> return (Just VALID, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.SocialSecurityNumber -> do
      mbSSN <- QDSSN.findByDriverId driverId
      return (mapStatus <$> (mbSSN <&> (.verificationStatus)), mbSSN >>= (.rejectReason), Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.ProfilePhoto -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.ProfilePhoto
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.UploadProfile -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.UploadProfile
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.PanCard -> do
      mbPanCard <- QDPC.findByDriverId driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbPanCard
          iid2 = mbPanCard >>= (.documentImageId2) <&> (.getId)
          reason = if (mbPanCard <&> (.verificationStatus)) == Just Documents.INVALID then ((mbPanCard >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbPanCard <&> (.documentImageId1)) >>= lookupImageFailReason) else Nothing
      mbPanMetadata <-
        if enableMetadata
          then forM mbPanCard $ \pan -> do
            panNumberDec <- decrypt pan.panCardNumber
            pure $ PanMetadata PanDocumentMetadata {panNumber = panNumberDec, panDocType = pan.docType, driverDob = pan.driverDob}
          else pure Nothing
      return (mapStatus . (.verificationStatus) <$> mbPanCard, reason, Nothing, Nothing, s3, iid, iid2, mbPanMetadata)
    DVC.GSTCertificate -> do
      mbGSTCertificate <- QDGST.findByDriverId driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbGSTCertificate
          iid2 = mbGSTCertificate >>= (.documentImageId2) <&> (.getId)
          reason = if (mbGSTCertificate <&> (.verificationStatus)) == Just Documents.INVALID then ((mbGSTCertificate >>= (.rejectReason)) >>= nonEmptyReason) <|> ((mbGSTCertificate <&> (.documentImageId1)) >>= lookupImageFailReason) else Nothing
      mbGstMetadata <-
        if enableMetadata
          then forM mbGSTCertificate $ \gst -> do
            gstNumberDec <- decrypt gst.gstin
            pure $ GSTMetadata GSTDocumentMetadata {gstNumber = gstNumberDec}
          else pure Nothing
      return (mapStatus . (.verificationStatus) <$> mbGSTCertificate, reason, Nothing, Nothing, s3, iid, iid2, mbGstMetadata)
    DVC.BackgroundVerification -> do
      mbBackgroundVerification <- BVQuery.findByDriverId driverId
      -- Expiry from BackgroundVerification table's expiresAt field (not from Image table)
      if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
        then return (Just VALID, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing, Nothing)
        else return (Nothing, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing, Nothing)
    DVC.DrivingSchoolCertificate -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.DrivingSchoolCertificate
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.PoliceVerificationCertificate -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.PoliceVerificationCertificate
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.LocalResidenceProof -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.LocalResidenceProof
      mbIdentityInfo <- QDII.findByDriverId driverId
      let hasAddressDetails =
            maybe False (\info -> isJust info.address && isJust info.addressDocumentType && isJust info.addressState) mbIdentityInfo
      let mbLocalMetadata =
            if enableMetadata
              then
                mbIdentityInfo <&> \info ->
                  LocalAddressProofMetadata LocalAddressProofDocumentMetadata {state = info.addressState, proofDocumentType = info.addressDocumentType, address = info.address}
              else Nothing
      let statusWithoutAddress = if isJust mbImageId then Just INVALID else Just NO_DOC_AVAILABLE
      return (if hasAddressDetails then status else statusWithoutAddress, reason, url, Nothing, mbS3Path, mbImageId, Nothing, mbLocalMetadata)
    DVC.DriverVehicleNOC -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.DriverVehicleNOC
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.TrainingForm -> do
      status <- checkLMSTrainingStatus driverId merchantOpCityId
      return (status, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.DriverInspectionHub -> do
      (status, reason) <- getInspectionHubStatusAndReason DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
      return (status, reason, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.OperatorPartnerCode -> do
      status <- getOperatorPartnerCodeStatus role driverId
      return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.BotApproval -> do
      status <- getBotApprovalStatusForPerson role driverId
      return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.UDYAMCertificate -> do
      mbUdyam <- QUDYAM.findByDriverId driverId
      case mbUdyam of
        Just udyam -> do
          mbUdyamMetadata <-
            if enableMetadata
              then do
                udyamNumberDec <- decrypt udyam.udyamNumber
                mbFoi <- QFOI.findByPrimaryKey driverId
                pure $ Just $ UDYAMMetadata UDYAMDocumentMetadata {udyamNumber = Just udyamNumberDec, tdsRate = mbFoi >>= (.tdsRate)}
              else pure Nothing
          return (Just $ mapStatus udyam.verificationStatus, udyam.rejectReason, Nothing, Nothing, mbS3Path, mbImageId, Nothing, mbUdyamMetadata)
        Nothing -> do
          let hasImage = not . null $ IQuery.filterImageByEntityIdAndImageTypeAndVerificationStatus entityImagesInfo DVC.UDYAMCertificate [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
          return (if hasImage then Just MANUAL_VERIFICATION_REQUIRED else Nothing, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.TANCertificate -> do
      mbDoc <- listToMaybe <$> QCommonDocExtra.findLatestByDriverIdAndDocumentType (Just driverId) DVC.TANCertificate
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.TANCertificate
      case mbDoc of
        Just doc -> do
          mbTanMetadata <-
            if enableMetadata
              then do
                mbFoi <- QFOI.findByPrimaryKey driverId
                pure $ Just $ TANMetadata TANDocumentMetadata {documentId = doc.id.getId, tdsRate = mbFoi >>= (.tdsRate)}
              else pure Nothing
          let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage doc.documentImageId
          return (Just (mapStatus doc.verificationStatus), doc.rejectReason <|> reason, url, Nothing, s3, iid, Nothing, mbTanMetadata)
        Nothing -> return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.LDCCertificate -> do
      mbDoc <- listToMaybe <$> QCommonDocExtra.findLatestByDriverIdAndDocumentType (Just driverId) DVC.LDCCertificate
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.LDCCertificate
      case mbDoc of
        Just doc -> do
          mbLdcMetadata <-
            if enableMetadata
              then do
                mbFoi <- QFOI.findByPrimaryKey driverId
                pure $ Just $ LDCMetadata LDCDocumentMetadata {documentId = doc.id.getId, tdsRate = mbFoi >>= (.tdsRate)}
              else pure Nothing
          let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage doc.documentImageId
          return (Just (mapStatus doc.verificationStatus), doc.rejectReason <|> reason, url, Nothing, s3, iid, Nothing, mbLdcMetadata)
        Nothing -> return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.BusinessLicense -> commonDocStatus DVC.BusinessLicense
    DVC.TaxiTransportLicense -> commonDocStatus DVC.TaxiTransportLicense
    DVC.BusinessRegistrationExtract -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.BusinessRegistrationExtract
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.TAXDetails -> commonDocStatus DVC.TAXDetails
    DVC.FinnishIDResidencePermit -> commonDocStatus DVC.FinnishIDResidencePermit
    DVC.TaxiDriverPermit -> commonDocStatus DVC.TaxiDriverPermit
    DVC.NomineeDetails -> do
      mbIdentityInfo <- QDII.findByDriverId driverId
      let hasNominee = maybe False (\info -> isJust info.nomineeName && isJust info.nomineeRelationship && isJust info.nomineeDob) mbIdentityInfo
      return (if hasNominee then Just VALID else Nothing, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing, Nothing)
    DVC.FleetRegistration -> do
      mbRegisteredAt <-
        if isFleetRole role
          then (.registeredAt) <$> (QFOI.findByPrimaryKey driverId >>= fromMaybeM (PersonNotFound driverId.getId))
          else pure Nothing
      return (VALID <$ mbRegisteredAt, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.BankingDetails -> do
      hasBankingDetails <-
        if isFleetRole role
          then maybe False (isJust . (.payoutVpa)) <$> QFOI.findByPrimaryKey driverId
          else maybe False (\di -> isJust di.driverBankAccountDetails || isJust di.payerVpa) <$> DIQuery.findById (cast driverId)
      return (if hasBankingDetails then Just VALID else Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    _ -> commonDocStatus docType

callGetDLGetStatus :: Id DP.Person -> Id DMOC.MerchantOperatingCity -> Flow ()
callGetDLGetStatus driverId merchantOpCityId = do
  latestReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId DVC.DriverLicense
  whenJust latestReq $ \verificationReq -> do
    when (verificationReq.status == "pending" || verificationReq.status == "source_down_retrying") $ do
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
  | isFleetRole role = getOperatorPartnerCodeStatusForFleet personId
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

checkBackgroundVerificationStatus :: Id DP.Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
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
  DP.Role ->
  Id DP.Person ->
  IQuery.EntityImagesInfo ->
  DDVC.DocumentType ->
  [DVC.VehicleCategory] ->
  DocVerificationConfigs ->
  Flow (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text)
getInProgressDriverDocuments role driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs = do
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
        Left _fleetConfs -> True
  (status, mbReason, mbUrl) <- case docType of
    DDVC.DriverLicense -> checkIfUnderProgress entityImagesInfo DDVC.DriverLicense
    DDVC.BackgroundVerification -> checkBackgroundVerificationStatus driverId merchantId merchantOpCityId
    DDVC.AadhaarCard -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.AadhaarCard onlyImageLookup filteredDocVerificationConfigs
    DDVC.PanCard -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.PanCard onlyImageLookup filteredDocVerificationConfigs
    DDVC.GSTCertificate -> checkIfImageUploadedOrInvalidated role entityImagesInfo DDVC.GSTCertificate onlyImageLookup filteredDocVerificationConfigs
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      let mbImages = IQuery.filterRecentLatestByEntityIdAndImageType entityImagesInfo DDVC.ProfilePhoto
      return (maybe NO_DOC_AVAILABLE mapStatus (mbImages >>= (.verificationStatus)), Nothing, Nothing)
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
  return (status, mbReason, mbUrl, Nothing, mbS3Path, mbImageId, Nothing)

checkIfImageUploadedOrInvalidated :: DP.Role -> IQuery.EntityImagesInfo -> DDVC.DocumentType -> Bool -> DocVerificationConfigs -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated role entityImagesInfo docType onlyImageLookup allDocVerificationConfigs = do
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
      hasDocumentVerificationConfig =
        case allDocVerificationConfigs of
          Left fleetConfigs ->
            -- Per-docType role match for fleet roles; fall back to any config row for this docType (old behavior).
            let exactRoleConfigs = filter (\c -> c.documentType == docType && c.role == role) fleetConfigs
                fallbackConfigs = filter (\c -> c.documentType == docType) fleetConfigs
                effectiveConfigs = if isFleetRole role && not (null exactRoleConfigs) then exactRoleConfigs else fallbackConfigs
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

getAadhaarStatus :: Id DP.Person -> Flow (ResponseStatus, Maybe DAadhaarCard.AadhaarCard)
getAadhaarStatus personId = do
  mAadhaarCard <- QAadhaarCard.findByPrimaryKey personId
  case mAadhaarCard of
    Just aadhaarCard -> do
      if aadhaarCard.verificationStatus == Documents.VALID
        then return (VALID, Just aadhaarCard)
        else return (MANUAL_VERIFICATION_REQUIRED, Just aadhaarCard)
    Nothing -> return (NO_DOC_AVAILABLE, Nothing)

getDLAndStatus :: Id DP.Person -> IQuery.EntityImagesInfo -> Language -> Maybe Bool -> Flow (ResponseStatus, Maybe DL.DriverLicense, Text)
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

getRCAndStatus :: Id DP.Person -> IQuery.EntityImagesInfo -> Language -> Flow (ResponseStatus, Maybe RC.VehicleRegistrationCertificate, Text)
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

verificationStatusCheck :: ResponseStatus -> Language -> DVC.DocumentType -> Maybe [Text] -> Flow Text
verificationStatusCheck status language img mbReasons = do
  case (status, img) of
    (INVALID, DVC.DriverLicense) -> toVerificationMessage DLInvalid language
    (INVALID, DVC.VehicleRegistrationCertificate) -> do
      msg <- toVerificationMessage RCInvalid language
      addVerificationReasons language mbReasons msg
    _ -> toVerificationMessage DocumentValid language

addVerificationReasons :: Language -> Maybe [Text] -> Text -> Flow Text
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

checkIfInVerification :: Id DP.Person -> IQuery.EntityImagesInfo -> DVC.DocumentType -> Language -> Flow (ResponseStatus, Text)
checkIfInVerification driverId entityImagesInfo docType language = do
  let onboardingTryLimit = entityImagesInfo.transporterConfig.onboardingTryLimit
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId (docTypeToText docType)
  hvVerificationReq <- listToMaybe <$> HVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId docType
  let mbVerificationReqRecord = getLatestVerificationRecord idfyVerificationReq hvVerificationReq
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
  verificationStatusWithMessage onboardingTryLimit (length images) mbVerificationReqRecord language docType

verificationStatusWithMessage :: Int -> Int -> Maybe SDO.VerificationReqRecord -> Language -> DVC.DocumentType -> Flow (ResponseStatus, Text)
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

getMessageFromResponse :: Language -> Maybe Text -> Flow Text
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
      documentData = doc.documentData,
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

getDigilockerResponseCode :: Id DP.Person -> Flow (Maybe Text)
getDigilockerResponseCode driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ mbSession >>= (.responseCode)

getDigilockerDocStatusMap :: Id DP.Person -> Flow DocStatus.DocStatusMap
getDigilockerDocStatusMap driverId = do
  mbSession <- listToMaybe <$> QDV.findLatestByDriverId (Just 1) (Just 0) driverId
  pure $ maybe DocStatus.emptyDocStatusMap (.docStatus) mbSession

mapDigilockerToResponseStatus :: DocStatus.DocStatusEnum -> Maybe ResponseStatus
mapDigilockerToResponseStatus DocStatus.DOC_PENDING = Just PENDING
mapDigilockerToResponseStatus DocStatus.DOC_FAILED = Just FAILED
mapDigilockerToResponseStatus DocStatus.DOC_CONSENT_DENIED = Just CONSENT_DENIED
mapDigilockerToResponseStatus DocStatus.DOC_PULL_REQUIRED = Just PULL_REQUIRED
mapDigilockerToResponseStatus DocStatus.DOC_SUCCESS = Just VALID
