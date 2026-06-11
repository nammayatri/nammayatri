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
    checkAllVehicleDocsVerifiedForRC,
    checkAllDriverDocsVerifiedForDriver,
    validateMandatoryVehicleDocsForRC,
    activateRCAutomatically,
    mkCommonDocumentItem,
    checkInspectionHubRequestCreated,
    getInspectionHubStatusForResponseStatus,
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
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.Digilocker as SDDigilocker
import SharedLogic.DriverOnboarding.VehicleDocs
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified Storage.Beam.IssueManagement ()
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.FleetOwnerDocumentVerificationConfig as CQFODVC
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.BackgroundVerification as BVQuery
import qualified Storage.Queries.CommonDriverOnboardingDocumentsExtra as QCommonDocExtra
import qualified Storage.Queries.DigilockerVerification as QDV
import qualified Storage.Queries.DriverGstin as QDGST
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverInformation.Internal as DIIQuery
import qualified Storage.Queries.DriverInformationExtra as DIQueryExtra
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as QDPC
import qualified Storage.Queries.DriverRCAssociation as DRAQuery
import qualified Storage.Queries.DriverSSN as QDSSN
import qualified Storage.Queries.DriverUdyam as QUDYAM
import qualified Storage.Queries.FleetDriverAssociationExtra as QFDA
import qualified Storage.Queries.FleetOwnerInformation as QFOI
import qualified Storage.Queries.HyperVergeVerification as HVQuery
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as IQuery
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.RideExtra as QRideExtra
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as RCQuery
import qualified Tools.BackgroundVerification as BackgroundVerification
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
    documentExpiry :: Maybe UTCTime
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

-- Check only vehicle docs for a specific RC (used for vehicle inspection approval)
-- mbPerson = Nothing for vehicle inspection, without driver linked to vehicle
checkAllVehicleDocsVerifiedForRC ::
  RC.VehicleRegistrationCertificate ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Text ->
  Flow Bool
checkAllVehicleDocsVerifiedForRC rc merchantOperatingCity transporterConfig language reqRegistrationNo = do
  let onlyMandatoryDocs = Just True
  let entity = IQuery.VehicleRCEntity rc
  entityImages <- IQuery.findAllByEntityId transporterConfig entity
  now <- getCurrentTime
  let entityImagesInfo = IQuery.EntityImagesInfo {entity, merchantOperatingCity, entityImages, transporterConfig, now}
  allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing
  let skipMessages = True -- Skip translations, only need status check for inspection
  vehicleDocumentsUnverified <- fetchVehicleDocuments entityImagesInfo allDocumentVerificationConfigs language (Just reqRegistrationNo) onlyMandatoryDocs skipMessages
  vehicleDoc <-
    find (\doc -> doc.registrationNo == reqRegistrationNo) vehicleDocumentsUnverified
      & fromMaybeM (InvalidRequest $ "Vehicle doc not found for vehicle with registartionNo " <> reqRegistrationNo)
  let makeSelfieAadhaarPanMandatory = Nothing
  pure $ checkAllVehicleDocsVerified allDocumentVerificationConfigs vehicleDoc makeSelfieAadhaarPanMandatory

-- Check only driver docs (used for driver inspection approval)
checkAllDriverDocsVerifiedForDriver ::
  DP.Person ->
  DMOC.MerchantOperatingCity ->
  DTC.TransporterConfig ->
  Language ->
  Flow Bool
checkAllDriverDocsVerifiedForDriver person merchantOperatingCity transporterConfig language = do
  let onlyMandatoryDocs = Just True
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
  let makeSelfieAadhaarPanMandatory = Nothing
      vehicleCategory = case vehicleDocumentsUnverified of
        (doc : _) -> fromMaybe doc.userSelectedVehicleCategory doc.verifiedVehicleCategory
        [] -> DVC.CAR
  pure $ checkAllDriverDocsVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory

refreshVehicleDocsVerificationStatusForRC ::
  Maybe DTC.TransporterConfig ->
  Id RC.VehicleRegistrationCertificate ->
  Flow ()
refreshVehicleDocsVerificationStatusForRC mbTransporterConfig rcId = do
  rc <- RCQuery.findById rcId >>= fromMaybeM (InternalError $ "RC not found by id " <> rcId.getId)
  merchantOpCityId <- rc.merchantOperatingCityId & fromMaybeM (InternalError $ "merchantOperatingCityId missing for RC " <> rc.id.getId)
  transporterConfig <-
    maybe
      (SCTC.findByMerchantOpCityId merchantOpCityId Nothing >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId))
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
    allDocumentVerificationConfigs <- CQDVC.findAllByMerchantOpCityId merchantOperatingCity.id Nothing
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
      (SCTC.findByMerchantOpCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (TransporterConfigNotFound person.merchantOperatingCityId.getId))
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
      then Left <$> CQFODVC.findAllByMerchantOpCityId merchantOpCityId Nothing
      else Right <$> CQDVC.findAllByMerchantOpCityId merchantOpCityId Nothing
  let driverDocConfigs = fromRight [] allDocVerificationConfigs :: [DVC.DocumentVerificationConfig]
  vehicleDocumentsUnverified <-
    if isFleetRole person.role
      then pure []
      else fetchVehicleDocuments entityImagesInfo driverDocConfigs language mbReqRegistrationNo onlyMandatoryDocs skipMessages
  pure VehicleDocsContext {allDocVerificationConfigs, driverDocConfigs, vehicleDocumentsUnverified}

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

  -- Conditional logic based on separateDriverVehicleEnablement flag (fleet always uses separate enablement)
  vehicleDocuments <-
    if isFleetRole person.role || transporterConfig.separateDriverVehicleEnablement == Just True
      then do
        -- Fleet owner enablement/disablement (uses FleetOwnerInformation)
        when (isFleetRole person.role) $ do
          let vehicleCategory = DVC.CAR
              allFleetDocsVerified = checkAllDriverDocsVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
              isRejectedMandatoryFleetDoc doc =
                doc.verificationStatus `elem` [FAILED, INVALID]
                  && not (checkIfDocumentValid allDocVerificationConfigs person.role doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory)

          -- First check if fleet should be disabled (has rejected mandatory docs)
          when (any isRejectedMandatoryFleetDoc driverDocuments && transporterConfig.allowDisableFleetOnRejectionDoc == Just True) $
            disableFleetOwnerOnRejectionDoc personId

          -- Then check if fleet should be enabled (all mandatory docs valid)
          when allFleetDocsVerified $
            enableDriver merchantOpCityId personId person.role Nothing transporterConfig merchantId
        -- Check driver enablement separately (only driver docs + driver inspection)
        when (person.role == DP.DRIVER) $ do
          let vehicleCategory = fromMaybe DVC.CAR $ onboardingVehicleCategory <|> listToMaybe possibleVehicleCategories
              allDriverDocsVerified = checkAllDriverDocsVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
          when allDriverDocsVerified $ do
            driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
            let driverInspectionNotRequired = transporterConfig.requiresDriverOnboardingInspection /= Just True || driverInfo.approved == Just True
                -- Allow first-time auto-enable even when dontAutoEnableDriver=true (enabledAt=Nothing means never enabled before)
                autoEnableAllowed = not (fromMaybe False transporterConfig.dontAutoEnableDriver) || isNothing driverInfo.enabledAt
            when (driverInspectionNotRequired && autoEnableAllowed) $ do
              enableDriver merchantOpCityId personId person.role (mDL >>= (.driverName)) transporterConfig merchantId
              whenJust onboardingVehicleCategory $ \category -> do
                DIIQuery.updateOnboardingVehicleCategory (Just category) personId
        -- Check vehicle enablement separately (only vehicle docs + vehicle inspection)
        getVehicleDocuments driverDocConfigs person.role vehicleDocumentsUnverified transporterConfig.requiresOnboardingInspection transporterConfig.vehicleCategoryExcludedFromVerification True driverDocuments merchantOpCityId
      else do
        -- Combined enablement: Check both driver and vehicle docs together (old behavior)
        whenJust vehicleCategoryWithoutMandatoryConfigs $ \vehicleCategory -> do
          let allDriverDocsVerified = checkAllDriverDocsVerified allDocVerificationConfigs person.role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory
          when (allDriverDocsVerified && transporterConfig.requiresOnboardingInspection /= Just True && person.role == DP.DRIVER) $ do
            driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
            let autoEnableAllowed = not (fromMaybe False transporterConfig.dontAutoEnableDriver) || isNothing driverInfo.enabledAt
            when autoEnableAllowed $ do
              enableDriver merchantOpCityId personId person.role (mDL >>= (.driverName)) transporterConfig merchantId
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
            getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories person.role (Just True)
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
        let allVehicleDocsVerified = checkAllVehicleDocsVerified driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory
            inspectionNotRequired = requiresOnboardingInspection /= Just True || vehicleDoc.isApproved
            isVehicleCategoryExcludedFromVerification = (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) `elem` (fromMaybe [] vehicleCategoryExcludedFromVerification)
            -- When separated: only check vehicle docs. When combined: check both driver and vehicle docs
            allDriverDocsVerified = separateEnablement || checkAllDriverDocsVerified (Right driverDocConfs) role driverDocuments (fromMaybe userSelectedVehicleCategory verifiedVehicleCategory) makeSelfieAadhaarPanMandatory
            -- Vehicle activation logic depends on enablement mode
            checkToActivateRC =
              if separateEnablement
                then (allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification
                else ((allVehicleDocsVerified && inspectionNotRequired && role == DP.DRIVER) || isVehicleCategoryExcludedFromVerification) && allDriverDocsVerified

        -- Activate RC if vehicle docs are verified and inspection is not required/approved
        -- isActive=False means RC was explicitly deactivated — skip auto-reactivation
        mbVehicle <- QVehicle.findById personId
        when (shouldActivateRc && isNothing mbVehicle && checkToActivateRC && role == DP.DRIVER && (isActive || not (fromMaybe False entityImagesInfo.transporterConfig.dontAutoEnableDriver))) $ do
          void $ withTryCatch "activateRCAutomatically:statusHandler" (activateRCAutomatically personId entityImagesInfo.merchantOperatingCity vehicleDoc.registrationNo)
          -- Enable driver when RC is activated (only when flow is NOT separated)
          -- When separated, driver enablement is handled separately in the driver enablement section
          unless separateEnablement $ do
            when (checkToActivateRC && not (fromMaybe False entityImagesInfo.transporterConfig.dontAutoEnableDriver)) $ do
              case (isVehicleCategoryExcludedFromVerification, mDL) of
                (True, _) -> enableDriver merchantOpCityId personId role Nothing entityImagesInfo.transporterConfig entityImagesInfo.merchantOperatingCity.merchantId
                (False, Just dl) -> enableDriver merchantOpCityId personId role dl.driverName entityImagesInfo.transporterConfig entityImagesInfo.merchantOperatingCity.merchantId
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
            documentExpiry = Just rc.fitnessExpiry -- Fitness expiry = RC expiry
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

  digilockerDocStatusMap <- if isDigiLockerEnabled then getDigilockerDocStatusMap driverId else pure DocStatus.emptyDocStatusMap

  driverDocumentTypes <- getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs
  driverDocumentTypes `forM` \docType -> do
    let mbDocStatus = if isDigiLockerEnabled then DocStatus.getDocStatus docType digilockerDocStatusMap else Nothing
        responseCode = mbDocStatus >>= (.responseCode)
        mbDocVerificationStatus = mbDocStatus >>= (mapDigilockerToResponseStatus . (.status))

    (mbProcessedStatus, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2) <- getProcessedDriverDocuments person.id entityImagesInfo docType useHVSdkForDL
    (status, mbReason, mbUrl, mbExpiryFinal, mbS3PathFinal, mbImageIdFinal, mbImageId2Final) <- case mbProcessedStatus of
      Just VALID -> pure (VALID, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2)
      Just s -> pure (s, mbProcessedReason, mbProcessedUrl, mbExpiry, mbS3Path, mbImageId, mbImageId2)
      Nothing -> case mbDocVerificationStatus of
        Just docStatus -> pure (docStatus, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
        Nothing -> getInProgressDriverDocuments driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs

    mbMessage <- documentStatusMessage status mbReason docType mbUrl language skipMessages
    let finalMessage = mbReason <|> (if isDigiLockerEnabled then responseCode else Nothing) <|> mbMessage
    return $ DocumentStatusItem {documentType = docType, verificationStatus = status, verificationMessage = finalMessage, verificationUrl = mbUrl, s3Path = mbS3PathFinal, imageId = mbImageIdFinal, imageId2 = mbImageId2Final, documentExpiry = mbExpiryFinal}



getDriverDocTypes ::
  Id DMOC.MerchantOperatingCity ->
  DocVerificationConfigs ->
  [DVC.VehicleCategory] ->
  DP.Role ->
  Maybe Bool ->
  Flow [DVC.DocumentType]
getDriverDocTypes merchantOpCityId allDocVerificationConfigs possibleVehicleCategories role onlyMandatoryDocs = do
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
          mandatoryDocTypes = nub $ map (.documentType) $ filter (.isMandatory) effectiveConfigs
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




checkAllVehicleDocsVerified ::
  [DVC.DocumentVerificationConfig] ->
  VehicleDocumentItem ->
  Maybe Bool ->
  Bool
checkAllVehicleDocsVerified driverDocConfs vehicleDoc makeSelfieAadhaarPanMandatory = do
  all (\doc -> checkIfDocumentValid (Right driverDocConfs) DP.DRIVER doc.documentType (fromMaybe vehicleDoc.userSelectedVehicleCategory vehicleDoc.verifiedVehicleCategory) doc.verificationStatus makeSelfieAadhaarPanMandatory) vehicleDoc.documents

checkAllDriverDocsVerified ::
  DocVerificationConfigs ->
  DP.Role ->
  [DocumentStatusItem] ->
  DVC.VehicleCategory ->
  Maybe Bool ->
  Bool
checkAllDriverDocsVerified allDocVerificationConfigs role driverDocuments vehicleCategory makeSelfieAadhaarPanMandatory = do
  all (\doc -> checkIfDocumentValid allDocVerificationConfigs role doc.documentType vehicleCategory doc.verificationStatus makeSelfieAadhaarPanMandatory) driverDocuments

enableDriver :: Id DMOC.MerchantOperatingCity -> Id DP.Person -> DP.Role -> Maybe Text -> DTC.TransporterConfig -> Id DM.Merchant -> Flow ()
enableDriver merchantOpCityId personId role driverName transporterConfig merchantId = do
  if isFleetRole role
    then do
      fleetOwnerInfo <- QFOI.findByPrimaryKey personId >>= fromMaybeM (PersonNotFound personId.getId)
      unless (fleetOwnerInfo.enabled && fleetOwnerInfo.verified) $ do
        QFOI.updateFleetOwnerEnabledAndVerifiedStatus True True personId
        cascadeFleetEnableToDrivers personId
    else do
      driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
      unless (driverInfo.enabled && driverInfo.verified) $ do
        SDO.enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId True
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

checkIfDocumentValid ::
  DocVerificationConfigs ->
  DP.Role ->
  DDVC.DocumentType ->
  DVC.VehicleCategory ->
  ResponseStatus ->
  Maybe Bool ->
  Bool
checkIfDocumentValid _ _ _ _ VALID _ = True
checkIfDocumentValid (Left fleetConfigs) role docType _category status _makeSelfieAadhaarPanMandatory = do
  let mbConfig = find (\config -> config.documentType == docType && config.role == role) fleetConfigs
  case mbConfig of
    Just config ->
      not config.isMandatory
        || ( case status of
               MANUAL_VERIFICATION_REQUIRED -> config.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True
checkIfDocumentValid (Right driverConfigs) _role docType category status makeSelfieAadhaarPanMandatory = do
  let mbVerificationConfig = find (\config -> config.documentType == docType && config.vehicleCategory == category) driverConfigs
  case mbVerificationConfig of
    Just verificationConfig ->
      not (fromMaybe verificationConfig.isMandatory verificationConfig.isMandatoryForEnabling && (not (fromMaybe False verificationConfig.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory))
        || ( case status of
               MANUAL_VERIFICATION_REQUIRED -> verificationConfig.isDefaultEnabledOnManualVerification
               _ -> False
           )
    Nothing -> True

getProcessedDriverDocuments :: Id DP.Person -> IQuery.EntityImagesInfo -> DVC.DocumentType -> Maybe Bool -> Flow (Maybe ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text)
getProcessedDriverDocuments driverId entityImagesInfo docType useHVSdkForDL = do
  let merchantOpCityId = entityImagesInfo.merchantOperatingCity.id
      (mbS3Path, mbImageId) = getImageMetaFromLatestImage entityImagesInfo docType
      lookupImage imgId =
        let mbImg = find (\img -> img.id == imgId) entityImagesInfo.entityImages
            iid = Just imgId.getId
         in (mbImg <&> (.s3Path), iid)
      commonDocStatus dt = do
        mbDoc <- listToMaybe <$> QCommonDocExtra.findLatestByDriverIdAndDocumentType (Just driverId) dt
        let (status, reason, url) = checkImageValidity entityImagesInfo dt
        case mbDoc of
          Just doc ->
            let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage doc.documentImageId
             in return (Just (mapStatus doc.verificationStatus), doc.rejectReason <|> reason, url, Nothing, s3, iid, Nothing)
          Nothing -> return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
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
          return (mapStatus <$> (mbDL' <&> (.verificationStatus)), mbDL' >>= (.rejectReason), Nothing, mbDL' <&> (.licenseExpiry), s3, iid, iid2)
        else do
          let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbDL
              iid2 = mbDL >>= (.documentImageId2) <&> (.getId)
          return (mapStatus <$> (mbDL <&> (.verificationStatus)), mbDL >>= (.rejectReason), Nothing, mbDL <&> (.licenseExpiry), s3, iid, iid2)
    DVC.AadhaarCard -> do
      mbAadhaarCard <- QAadhaarCard.findByPrimaryKey driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) lookupImage (mbAadhaarCard >>= (.aadhaarFrontImageId))
          iid2 = mbAadhaarCard >>= (.aadhaarBackImageId) <&> (.getId)
      return (mapStatus . (.verificationStatus) <$> mbAadhaarCard, Nothing, Nothing, Nothing, s3, iid, iid2)
    DVC.Permissions -> return (Just VALID, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.SocialSecurityNumber -> do
      mbSSN <- QDSSN.findByDriverId driverId
      return (mapStatus <$> (mbSSN <&> (.verificationStatus)), mbSSN >>= (.rejectReason), Nothing, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.ProfilePhoto -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.ProfilePhoto
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.UploadProfile -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.UploadProfile
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.PanCard -> do
      mbPanCard <- QDPC.findByDriverId driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbPanCard
          iid2 = mbPanCard >>= (.documentImageId2) <&> (.getId)
      return (mapStatus . (.verificationStatus) <$> mbPanCard, Nothing, Nothing, Nothing, s3, iid, iid2)
    DVC.GSTCertificate -> do
      mbGSTCertificate <- QDGST.findByDriverId driverId
      let (s3, iid) = maybe (mbS3Path, mbImageId) (lookupImage . (.documentImageId1)) mbGSTCertificate
          iid2 = mbGSTCertificate >>= (.documentImageId2) <&> (.getId)
      return (mapStatus . (.verificationStatus) <$> mbGSTCertificate, Nothing, Nothing, Nothing, s3, iid, iid2)
    DVC.BackgroundVerification -> do
      mbBackgroundVerification <- BVQuery.findByDriverId driverId
      -- Expiry from BackgroundVerification table's expiresAt field (not from Image table)
      if (mbBackgroundVerification <&> (.reportStatus)) == Just Documents.VALID
        then return (Just VALID, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing)
        else return (Nothing, Nothing, Nothing, mbBackgroundVerification <&> (.expiresAt), mbS3Path, mbImageId, Nothing)
    DVC.DrivingSchoolCertificate -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.DrivingSchoolCertificate
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.PoliceVerificationCertificate -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.PoliceVerificationCertificate
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.LocalResidenceProof -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.LocalResidenceProof
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.TrainingForm -> do
      status <- checkLMSTrainingStatus driverId merchantOpCityId
      return (status, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.DriverInspectionHub -> do
      status <- getInspectionHubStatusForResponseStatus DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
      return (status, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)
    DVC.UDYAMCertificate -> do
      mbUdyam <- QUDYAM.findByDriverId driverId
      case mbUdyam of
        Just udyam -> return (Just $ mapStatus udyam.verificationStatus, udyam.rejectReason, Nothing, Nothing, mbS3Path, mbImageId, Nothing)
        Nothing -> do
          let hasImage = not . null $ IQuery.filterImageByEntityIdAndImageTypeAndVerificationStatus entityImagesInfo DVC.UDYAMCertificate [Documents.VALID, Documents.MANUAL_VERIFICATION_REQUIRED]
          return (if hasImage then Just MANUAL_VERIFICATION_REQUIRED else Nothing, Nothing, Nothing, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.TANCertificate -> commonDocStatus DVC.TANCertificate
    DVC.LDCCertificate -> commonDocStatus DVC.LDCCertificate
    DVC.BusinessLicense -> commonDocStatus DVC.BusinessLicense
    DVC.TaxiTransportLicense -> commonDocStatus DVC.TaxiTransportLicense
    DVC.BusinessRegistrationExtract -> do
      let (status, reason, url) = checkImageValidity entityImagesInfo DVC.BusinessRegistrationExtract
      return (status, reason, url, Nothing, mbS3Path, mbImageId, Nothing)
    DVC.TAXDetails -> commonDocStatus DVC.TAXDetails
    DVC.FinnishIDResidencePermit -> commonDocStatus DVC.FinnishIDResidencePermit
    DVC.TaxiDriverPermit -> commonDocStatus DVC.TaxiDriverPermit
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
  Id DP.Person ->
  IQuery.EntityImagesInfo ->
  DDVC.DocumentType ->
  [DVC.VehicleCategory] ->
  DocVerificationConfigs ->
  Flow (ResponseStatus, Maybe Text, Maybe BaseUrl, Maybe UTCTime, Maybe Text, Maybe Text, Maybe Text)
getInProgressDriverDocuments driverId entityImagesInfo docType possibleVehicleCategories allDocVerificationConfigs = do
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
    DDVC.AadhaarCard -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.AadhaarCard onlyImageLookup filteredDocVerificationConfigs
    DDVC.PanCard -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.PanCard onlyImageLookup filteredDocVerificationConfigs
    DDVC.GSTCertificate -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.GSTCertificate onlyImageLookup filteredDocVerificationConfigs
    DDVC.Permissions -> return (VALID, Nothing, Nothing)
    DDVC.ProfilePhoto -> do
      let mbImages = IQuery.filterRecentLatestByEntityIdAndImageType entityImagesInfo DDVC.ProfilePhoto
      return (fromMaybe NO_DOC_AVAILABLE (mapStatus <$> (mbImages >>= (.verificationStatus))), Nothing, Nothing)
    DDVC.UploadProfile -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.UploadProfile onlyImageLookup filteredDocVerificationConfigs
    DDVC.DrivingSchoolCertificate -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.DrivingSchoolCertificate onlyImageLookup filteredDocVerificationConfigs
    DDVC.PoliceVerificationCertificate -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.PoliceVerificationCertificate onlyImageLookup filteredDocVerificationConfigs
    DDVC.LocalResidenceProof -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.LocalResidenceProof onlyImageLookup filteredDocVerificationConfigs
    DDVC.TrainingForm -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.TrainingForm onlyImageLookup filteredDocVerificationConfigs
    DDVC.DriverInspectionHub -> do
      mbStatus <- getInspectionHubStatusForResponseStatus DOHR.DRIVER_ONBOARDING_INSPECTION (Just driverId) Nothing
      let status = fromMaybe INVALID mbStatus
      return (status, Nothing, Nothing)
    DDVC.BusinessLicense -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.BusinessLicense onlyImageLookup filteredDocVerificationConfigs
    DDVC.TaxiTransportLicense -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.TaxiTransportLicense onlyImageLookup filteredDocVerificationConfigs
    DDVC.BusinessRegistrationExtract -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.BusinessRegistrationExtract onlyImageLookup filteredDocVerificationConfigs
    DDVC.TAXDetails -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.TAXDetails onlyImageLookup filteredDocVerificationConfigs
    DDVC.FinnishIDResidencePermit -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.FinnishIDResidencePermit onlyImageLookup filteredDocVerificationConfigs
    DDVC.TANCertificate -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.TANCertificate onlyImageLookup filteredDocVerificationConfigs
    DDVC.LDCCertificate -> checkIfImageUploadedOrInvalidated entityImagesInfo DDVC.LDCCertificate onlyImageLookup filteredDocVerificationConfigs
    _ -> return (NO_DOC_AVAILABLE, Nothing, Nothing)
  return (status, mbReason, mbUrl, Nothing, mbS3Path, mbImageId, Nothing)





checkIfImageUploadedOrInvalidated :: IQuery.EntityImagesInfo -> DDVC.DocumentType -> Bool -> DocVerificationConfigs -> Flow (ResponseStatus, Maybe Text, Maybe BaseUrl)
checkIfImageUploadedOrInvalidated entityImagesInfo docType onlyImageLookup allDocVerificationConfigs = do
  let images = IQuery.filterRecentByEntityIdAndImageType entityImagesInfo docType
      hasDocumentVerificationConfig =
        case allDocVerificationConfigs of
          Left fleetConfigs ->
            any
              (\config -> config.documentType == docType && not config.isDefaultEnabledOnManualVerification)
              fleetConfigs
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
  idfyVerificationReq <- listToMaybe <$> IVQuery.findLatestByDriverIdAndDocType (Just 1) Nothing driverId docType
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



