{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.DriverOnboarding
  ( module SharedLogic.DriverOnboarding,
    module Reexport,
  )
where

import qualified API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding
import qualified API.Types.ProviderPlatform.Fleet.Onboarding
import qualified API.Types.ProviderPlatform.Management.Endpoints.Account
import qualified API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration
import AWS.S3 as S3
import Control.Applicative ((<|>))
import qualified Data.List as DL
import qualified Data.Text as T
import Data.Time hiding (getCurrentTime, secondsToNominalDiffTime)
import qualified Data.Time.Calendar.OrdinalDate as TO
import qualified Domain.Types as DVST
import qualified Domain.Types.DocsVerificationStatus as DDVS
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.DocumentVerificationConfig as DVC
import qualified Domain.Types.DocumentVerificationConfig as ODC
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.DriverPanCard as DPan
import Domain.Types.DriverRCAssociation
import Domain.Types.Extra.IdfyVerification (docTypeToText, faceCompareDocTag, inProgressIdfyStatuses, parseDocType)
import qualified Domain.Types.FleetOwnerDocumentVerificationConfig
import qualified Domain.Types.FleetRCAssociation as FRCA
import qualified Domain.Types.HyperVergeVerification as DHV
import qualified Domain.Types.IdfyVerification as DIdfy
import qualified Domain.Types.Image as Domain
import qualified Domain.Types.Image as Image
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.Merchant as DTM
import qualified Domain.Types.MerchantMessage as DMM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import qualified Domain.Types.Person as Person
import qualified Domain.Types.TransporterConfig as DTC
import Domain.Types.Vehicle as DV
import qualified Domain.Types.VehicleCategory as DVC
import Domain.Types.VehicleRegistrationCertificate
import qualified Domain.Types.VehicleServiceTier as DVST
import qualified Domain.Types.VehicleVariant as DV
import Domain.Utils as Reexport
import Environment
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.External.Ticket.Interface.Types as Ticket
import Kernel.External.Types (Language, VerificationFlow)
import qualified Kernel.External.Verification.Interface as VI
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Documents
import qualified Kernel.Types.Documents as Documents
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import qualified SharedLogic.Allocator.Jobs.Overlay.SendOverlay as ACOverlay
import SharedLogic.Analytics as Analytics
import qualified SharedLogic.Association.Change as AC
import SharedLogic.MessageBuilder (addBroadcastMessageToKafka)
import SharedLogic.VehicleServiceTier
import qualified Storage.Cac.TransporterConfig as SCTC
import qualified Storage.CachedQueries.DocumentVerificationConfig as CQDVC
import qualified Storage.CachedQueries.Merchant as CQM
import qualified Storage.CachedQueries.Merchant.MerchantMessage as QMM
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC
import Storage.ConfigPilot.Config.DocumentVerificationConfig (DocumentVerificationConfigDimensions (..))
import Storage.ConfigPilot.Config.Translation (TranslationDimensions (..))
import Storage.ConfigPilot.Config.TransporterConfig (TransporterConfigDimensions (..))
import qualified Storage.Queries.AadhaarCard as QAadhaarCard
import qualified Storage.Queries.DriverGstin as DGQuery
import qualified Storage.Queries.DriverInformation as DIQuery
import qualified Storage.Queries.DriverLicense as DLQuery
import qualified Storage.Queries.DriverPanCard as DPQuery
import qualified Storage.Queries.DriverRCAssociation as DAQuery
import qualified Storage.Queries.FleetDriverAssociation as QFDA
import qualified Storage.Queries.FleetOwnerInformation as FOI
import qualified Storage.Queries.FleetRCAssociation as FRCAssoc
import qualified Storage.Queries.IdfyVerification as IVQuery
import qualified Storage.Queries.Image as ImageQuery
import qualified Storage.Queries.Image as Query
import qualified Storage.Queries.Message as MessageQuery
import qualified Storage.Queries.Person as QP
import qualified Storage.Queries.Translations as MTQuery
import qualified Storage.Queries.Vehicle as QVehicle
import qualified Storage.Queries.VehicleRegistrationCertificate as QRC
import Text.Regex.Posix ((=~))
import Tools.Error
import qualified Tools.Ticket as TT
import qualified Tools.Verification as Verification
import qualified Tools.Whatsapp as Whatsapp

defaultDriverDocumentTypes :: [DVC.DocumentType]
defaultDriverDocumentTypes = [DVC.DriverLicense, DVC.AadhaarCard, DVC.PanCard, DVC.Permissions, DVC.ProfilePhoto, DVC.UploadProfile, DVC.SocialSecurityNumber, DVC.BackgroundVerification, DVC.GSTCertificate, DVC.BusinessLicense, DVC.LocalResidenceProof, DVC.PoliceVerificationCertificate, DVC.DrivingSchoolCertificate, DVC.TrainingForm, DVC.DriverInspectionHub, DVC.FinnishIDResidencePermit, DVC.TaxiDriverPermit]

defaultFleetDocumentTypes :: [DVC.DocumentType]
defaultFleetDocumentTypes = [DVC.AadhaarCard, DVC.PanCard, DVC.GSTCertificate, DVC.BusinessLicense, DVC.UDYAMCertificate, DVC.TANCertificate, DVC.LDCCertificate]

defaultVehicleDocumentTypes :: [DVC.DocumentType]
defaultVehicleDocumentTypes = [DVC.VehicleRegistrationCertificate, DVC.VehiclePermit, DVC.VehicleFitnessCertificate, DVC.VehicleInsurance, DVC.VehiclePUC, DVC.VehicleInspectionForm, DVC.SubscriptionPlan, DVC.VehicleLeft, DVC.VehicleRight, DVC.VehicleFrontInterior, DVC.VehicleBackInterior, DVC.VehicleFront, DVC.VehicleBack, DVC.Odometer, DVC.VehicleNOC, DVC.InspectionHub]

isFleetRole :: Person.Role -> Bool
isFleetRole Person.FLEET_OWNER = True
isFleetRole Person.FLEET_BUSINESS = True
isFleetRole _ = False

notifyErrorToSupport ::
  Person ->
  Id DTM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Maybe T.Text ->
  T.Text ->
  [Maybe DriverOnboardingError] ->
  Flow ()
notifyErrorToSupport person merchantId merchantOpCityId driverPhone _ errs = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  let reasons = catMaybes $ mapMaybe toMsg errs
  let description = T.intercalate ", " reasons
  _ <- TT.createTicket merchantId merchantOpCityId (mkTicket description transporterConfig)
  return ()
  where
    toMsg e = toMessage <$> e

    mkTicket description transporterConfig =
      Ticket.CreateTicketReq
        { category = "GENERAL",
          subCategory = Just "DRIVER ONBOARDING ISSUE",
          disposition = transporterConfig.kaptureDisposition,
          queue = transporterConfig.kaptureQueue,
          issueId = Nothing,
          issueDescription = description,
          mediaFiles = Nothing,
          name = Just $ person.firstName <> " " <> fromMaybe "" person.lastName,
          phoneNo = driverPhone,
          personId = person.id.getId,
          classification = Ticket.DRIVER,
          rideDescription = Nothing,
          ticketContext = Just Ticket.IssueTicket,
          becknIssueId = Nothing,
          xyneChannelId = Nothing
        }

throwImageError :: Id Domain.Image -> DriverOnboardingError -> Flow b
throwImageError id_ err = do
  _ <- Query.addFailureReason (Just err) id_
  throwError err

getFreeTrialDaysLeft :: MonadFlow m => Int -> DI.DriverInformation -> m Int
getFreeTrialDaysLeft freeTrialDays driverInfo = do
  now <- getCurrentTime
  let driverEnablementDay = utctDay (fromMaybe now (driverInfo.enabledAt <|> driverInfo.lastEnabledOn))
  return $ max 0 (freeTrialDays - fromInteger (diffDays (utctDay now) driverEnablementDay))

triggerOnboardingAlertsAndMessages :: Person -> DTM.Merchant -> DMOC.MerchantOperatingCity -> Flow ()
triggerOnboardingAlertsAndMessages driver merchant merchantOperatingCity = do
  fork "Triggering onboarding messages" $ do
    -- broadcast messages
    messages <- MessageQuery.findAllOnboardingMessages merchant merchantOperatingCity
    mapM_ (\msg -> addBroadcastMessageToKafka False msg driver.id) messages

    -- whatsapp message
    mobileNumber <- mapM decrypt driver.mobileNumber >>= fromMaybeM (PersonFieldNotPresent "mobileNumber")
    countryCode <- driver.mobileCountryCode & fromMaybeM (PersonFieldNotPresent "mobileCountryCode")
    let phoneNumber = countryCode <> mobileNumber
    merchantMessage <-
      QMM.findByMerchantOpCityIdAndMessageKeyVehicleCategory merchantOperatingCity.id DMM.WELCOME_TO_PLATFORM Nothing Nothing
        >>= fromMaybeM (MerchantMessageNotFound merchantOperatingCity.id.getId (show DMM.WELCOME_TO_PLATFORM))
    let jsonData = merchantMessage.jsonData
    result <- Whatsapp.whatsAppSendMessageWithTemplateIdAPI driver.merchantId merchantOperatingCity.id (Whatsapp.SendWhatsAppMessageWithTemplateIdApIReq phoneNumber merchantMessage.templateId [jsonData.var1, jsonData.var2, jsonData.var3] Nothing (Just merchantMessage.containsUrlButton)) -- Accepts at most 7 variables using GupShup
    when (result._response.status /= "success") $ throwError (InternalError "Unable to send Whatsapp message via dashboard")

enableAndTriggerOnboardingAlertsAndMessages :: Id DMOC.MerchantOperatingCity -> Id Person -> Bool -> Flow ()
enableAndTriggerOnboardingAlertsAndMessages merchantOpCityId personId verified = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  merchantOpCity <- CQMOC.findById merchantOpCityId >>= fromMaybeM (MerchantOperatingCityNotFound merchantOpCityId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  Analytics.updateEnabledVerifiedStateWithAnalytics (Just driverInfo) transporterConfig personId True (Just verified)
  DIQuery.updateDisabledReasonFlag Nothing (cast personId)
  when (not driverInfo.enabled && isNothing driverInfo.enabledAt) $ do
    merchant <- CQM.findById merchantOpCity.merchantId >>= fromMaybeM (MerchantNotFound merchantOpCity.merchantId.getId)
    person <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
    triggerOnboardingAlertsAndMessages person merchant merchantOpCity

-- | Set the driver's enabled state to False with analytics + LTS pool sync (the inverse of
--   'enableAndTriggerOnboardingAlertsAndMessages'). @mbVerified@ optionally co-writes `verified`
--   (Nothing leaves it untouched). Used by the enableBotFlow recompute when docs become invalid.
--   Under enableBotFlow the disable also revokes `approved` (handled inside the analytics helper).
disableDriverWithAnalytics :: Id DMOC.MerchantOperatingCity -> Id Person -> Maybe Bool -> Flow ()
disableDriverWithAnalytics merchantOpCityId personId mbVerified = do
  driverInfo <- DIQuery.findById (cast personId) >>= fromMaybeM (PersonNotFound personId.getId)
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  Analytics.updateEnabledVerifiedStateWithAnalytics (Just driverInfo) transporterConfig personId False mbVerified

checkAndUpdateAirConditioned :: Bool -> Bool -> Id Person -> Id DMOC.MerchantOperatingCity -> [DVST.VehicleServiceTier] -> Maybe Text -> Bool -> Flow ()
checkAndUpdateAirConditioned isDashboard isAirConditioned personId merchantOpCityId cityVehicleServiceTiers downgradeReason shouldUpdateServiceTiers = do
  driverInfo <- runInReplica $ DIQuery.findById personId >>= fromMaybeM DriverInfoNotFound
  vehicle <- runInReplica $ QVehicle.findById personId >>= fromMaybeM (VehicleNotFound personId.getId)
  let serviceTierACThresholds = map (\DVST.VehicleServiceTier {isAirConditioned = _a, ..} -> airConditionedThreshold) (filter (\v -> vehicle.variant `elem` v.allowedVehicleVariant) cityVehicleServiceTiers)

  when (isAirConditioned && not (checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds))) $ do
    when (driverInfo.acUsageRestrictionType == DI.ToggleNotAllowed) $
      if isDashboard
        then do
          DIQuery.removeAcUsageRestriction (Just 0.0) DI.ToggleNotAllowed (driverInfo.acRestrictionLiftCount + 1) personId
          driver <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
          fork "Send AC Restriction Lifted Overlay" $ ACOverlay.sendACUsageRestrictionLiftedOverlay driver
        else throwError $ InvalidRequest "AC usage is restricted for the driver, please contact support"
    when (driverInfo.acUsageRestrictionType `elem` [DI.ToggleAllowed, DI.NoRestriction]) $
      DIQuery.updateAcUsageRestrictionAndScore DI.ToggleNotAllowed (Just 0.0) personId
  mbRc <- runInReplica $ QRC.findLastVehicleRCWrapper vehicle.registrationNo
  QVehicle.updateAirConditioned (Just isAirConditioned) downgradeReason personId
  whenJust mbRc $ \rc -> QRC.updateAirConditioned (Just isAirConditioned) rc.id
  when shouldUpdateServiceTiers $ do
    let acRestricted = isAirConditioned && not (checkIfACAllowedForDriver driverInfo (catMaybes serviceTierACThresholds))
        driverInfo' = if acRestricted then driverInfo {DI.airConditionScore = Just 0.0} else driverInfo
        vehicle' = vehicle {DV.airConditioned = Just isAirConditioned}
    serviceTiers <- fetchVehicleTierForDriverWithUsageRestriction True (Just driverInfo') (Just vehicle') Nothing (Just cityVehicleServiceTiers) personId merchantOpCityId
    let newTiers = (.serviceTierType) . fst <$> filter (not . snd) serviceTiers
    QVehicle.updateSelectedServiceTiers newTiers personId

checkIfACAllowedForDriver :: DI.DriverInformation -> [Double] -> Bool
checkIfACAllowedForDriver driverInfo serviceTierACThresholds = null serviceTierACThresholds || any ((fromMaybe 0 driverInfo.airConditionScore) <=) serviceTierACThresholds

incrementDriverAcUsageRestrictionCount :: [DVST.VehicleServiceTier] -> Id DMOC.MerchantOperatingCity -> Id Person -> Flow ()
incrementDriverAcUsageRestrictionCount cityVehicleServiceTiers merchantOpCityId personId = do
  driverInfo <- DIQuery.findById personId >>= fromMaybeM DriverInfoNotFound
  driver <- QP.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let mbMaxACUsageRestrictionThreshold = safeMaximum . mapMaybe (\DVST.VehicleServiceTier {..} -> airConditionedThreshold) $ cityVehicleServiceTiers
  let airConditionScore = (fromMaybe 0 driverInfo.airConditionScore) + 1
  if maybe False (airConditionScore >) mbMaxACUsageRestrictionThreshold
    then do
      let newRestrictionType =
            if driverInfo.acUsageRestrictionType == DI.NoRestriction
              then DI.ToggleAllowed
              else driverInfo.acUsageRestrictionType
      DIQuery.updateAcUsageRestrictionAndScore newRestrictionType (Just airConditionScore) personId
      fork "Send AC Restriction Overlay" $ ACOverlay.sendACUsageRestrictionOverlay driver
    else do
      DIQuery.updateAirConditionScore (Just airConditionScore) personId
      whenJust mbMaxACUsageRestrictionThreshold $ \threshold -> do
        let thresholdInt = round threshold :: Int
            scoreInt = round airConditionScore :: Int
        when (scoreInt == thresholdInt - 1 || scoreInt == thresholdInt) $
          fork "Send AC Warning Overlay" $ ACOverlay.sendACUsageWarningOverlay driver
  let updatedDriverInfo = driverInfo {DI.airConditionScore = Just airConditionScore}
  serviceTiers <- fetchVehicleTierForDriverWithUsageRestriction True (Just updatedDriverInfo) Nothing Nothing (Just cityVehicleServiceTiers) personId merchantOpCityId
  let newTiers = (.serviceTierType) . fst <$> filter (not . snd) serviceTiers
  QVehicle.updateSelectedServiceTiers newTiers personId
  where
    safeMaximum :: Ord a => [a] -> Maybe a
    safeMaximum [] = Nothing
    safeMaximum xs = Just (maximum xs)

createDriverRCAssociationIfPossible ::
  forall m r.
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DTC.TransporterConfig ->
  Id Person ->
  VehicleRegistrationCertificate ->
  m ()
createDriverRCAssociationIfPossible transporterConfig driverId rc = do
  when (transporterConfig.blockDriverOwnRCForFleetDrivers == Just True && isNothing rc.fleetOwnerId) $ do
    mbFleetAssoc <- QFDA.findByDriverId driverId True
    whenJust mbFleetAssoc $ \_ ->
      throwError (InvalidRequest "Fleet drivers cannot onboard their own vehicle. Use a fleet vehicle.")
  if canCreateRCAssociation transporterConfig rc
    then do
      when (transporterConfig.blockDriverOwnRCForFleetDrivers == Just True) $
        AC.guardRCNotOwnedByAnotherFleet driverId rc.id
      driverRCAssoc <- makeRCAssociation transporterConfig.merchantId transporterConfig.merchantOperatingCityId rc.id defaultAssociationEnd
      DAQuery.create driverRCAssoc
    else do
      logWarning $ "Unable to create driver rc association: " <> "; driverId: " <> driverId.getId <> "; rcId: " <> rc.id.getId <> "; verification status: " <> show rc.verificationStatus
  where
    makeRCAssociation :: Id DTM.Merchant -> Id DMOC.MerchantOperatingCity -> Id VehicleRegistrationCertificate -> Maybe UTCTime -> m DriverRCAssociation
    makeRCAssociation merchantId merchantOperatingCityId rcId end = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        DriverRCAssociation
          { id,
            driverId,
            rcId,
            associatedOn = now,
            associatedTill = end,
            errorMessage = Nothing,
            consent = True,
            consentTimestamp = now,
            isRcActive = False,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

createFleetRCAssociationIfPossible ::
  forall m r.
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DTC.TransporterConfig ->
  Id Person ->
  VehicleRegistrationCertificate ->
  m ()
createFleetRCAssociationIfPossible transporterConfig fleetOwnerId rc = do
  if canCreateRCAssociation transporterConfig rc
    then do
      when (transporterConfig.blockDriverOwnRCForFleetDrivers == Just True) $
        AC.guardRCNotActiveWithAnotherDriver rc.id
      fleetRCAssoc <- makeFleetRCAssociation transporterConfig.merchantId transporterConfig.merchantOperatingCityId rc.id defaultAssociationEnd
      FRCAssoc.create fleetRCAssoc
    else do
      logWarning $ "Unable to create fleet rc association: " <> "; fleetOwnerId: " <> fleetOwnerId.getId <> "; rcId: " <> rc.id.getId <> "; verification status: " <> show rc.verificationStatus
  where
    makeFleetRCAssociation :: (MonadFlow m) => Id DTM.Merchant -> Id DMOC.MerchantOperatingCity -> Id VehicleRegistrationCertificate -> Maybe UTCTime -> m FRCA.FleetRCAssociation
    makeFleetRCAssociation merchantId merchantOperatingCityId rcId end = do
      id <- generateGUID
      now <- getCurrentTime
      return $
        FRCA.FleetRCAssociation
          { id,
            rcId,
            fleetOwnerId,
            associatedOn = now,
            associatedTill = end,
            merchantId = Just merchantId,
            merchantOperatingCityId = Just merchantOperatingCityId,
            createdAt = now,
            updatedAt = now
          }

canCreateRCAssociation :: DTC.TransporterConfig -> VehicleRegistrationCertificate -> Bool
canCreateRCAssociation transporterConfig rc =
  transporterConfig.requiresOnboardingInspection /= Just True
    || rc.verificationStatus == Documents.VALID
    || (rc.verificationStatus == Documents.INVALID && not (null rc.failedRules))

data VehicleRegistrationCertificateAPIEntity = VehicleRegistrationCertificateAPIEntity
  { certificateNumber :: Text,
    fitnessExpiry :: UTCTime,
    permitExpiry :: Maybe UTCTime,
    pucExpiry :: Maybe UTCTime,
    insuranceValidity :: Maybe UTCTime,
    vehicleClass :: Maybe Text,
    vehicleVariant :: Maybe DV.VehicleVariant,
    failedRules :: [Text],
    vehicleManufacturer :: Maybe Text,
    vehicleCapacity :: Maybe Int,
    vehicleModel :: Maybe Text,
    manufacturerModel :: Maybe Text,
    reviewRequired :: Maybe Bool,
    vehicleColor :: Maybe Text,
    vehicleEnergyType :: Maybe Text,
    reviewedAt :: Maybe UTCTime,
    verificationStatus :: VerificationStatus,
    fleetOwnerId :: Maybe Text,
    createdAt :: UTCTime
  }
  deriving (Generic, ToSchema, ToJSON, FromJSON)

makeRCAPIEntity :: VehicleRegistrationCertificate -> Text -> VehicleRegistrationCertificateAPIEntity
makeRCAPIEntity VehicleRegistrationCertificate {..} rcDecrypted =
  VehicleRegistrationCertificateAPIEntity
    { certificateNumber = rcDecrypted,
      ..
    }

makeFullVehicleFromRC :: [DVST.VehicleServiceTier] -> DI.DriverInformation -> Person -> Id DTM.Merchant -> Text -> VehicleRegistrationCertificate -> Id DMOC.MerchantOperatingCity -> UTCTime -> Maybe [Text] -> Vehicle
makeFullVehicleFromRC vehicleServiceTiers driverInfo driver merchantId_ certificateNumber rc merchantOpCityId now vehicleTag = do
  let vehicle = makeVehicleFromRC driver.id merchantId_ certificateNumber rc merchantOpCityId now vehicleTag
  let availableServiceTiersForDriver = (.serviceTierType) . fst <$> selectVehicleTierForDriverWithUsageRestriction True driverInfo vehicle vehicleServiceTiers Nothing now
  addSelectedServiceTiers availableServiceTiersForDriver vehicle
  where
    addSelectedServiceTiers :: [DVST.ServiceTierType] -> Vehicle -> Vehicle
    addSelectedServiceTiers serviceTiers Vehicle {..} = Vehicle {selectedServiceTiers = serviceTiers, ..}

makeVehicleFromRC :: Id Person -> Id DTM.Merchant -> Text -> VehicleRegistrationCertificate -> Id DMOC.MerchantOperatingCity -> UTCTime -> Maybe [Text] -> Vehicle
makeVehicleFromRC driverId merchantId certificateNumber rc merchantOpCityId now vehicleTag = do
  Vehicle
    { driverId,
      capacity = rc.vehicleCapacity,
      category = DV.castVehicleVariantToVehicleCategory <$> rc.vehicleVariant,
      make = rc.vehicleManufacturer,
      model = fromMaybe "Unknown" rc.vehicleModel,
      size = Nothing,
      merchantId,
      variant = fromMaybe DV.AUTO_RICKSHAW rc.vehicleVariant,
      color = fromMaybe "Unknown" rc.vehicleColor,
      energyType = rc.vehicleEnergyType,
      registrationNo = certificateNumber,
      registrationCategory = Nothing,
      vehicleClass = fromMaybe "Unknown" rc.vehicleClass,
      merchantOperatingCityId = Just merchantOpCityId,
      vehicleName = Nothing,
      airConditioned = rc.airConditioned,
      oxygen = rc.oxygen,
      ventilator = rc.ventilator,
      luggageCapacity = rc.luggageCapacity,
      vehicleRating = rc.vehicleRating,
      vehicleRatingRemark = rc.vehicleRatingRemark,
      mYManufacturing = rc.mYManufacturing,
      selectedServiceTiers = [],
      downgradeReason = Nothing,
      createdAt = now,
      updatedAt = now,
      vehicleTags = vehicleTag,
      ruleBasedUpgradeTiers = Nothing,
      vehicleImageId = rc.vehicleImageId
    }

makeVehicleAPIEntity :: Maybe DVST.ServiceTierType -> Vehicle -> VehicleAPIEntity
makeVehicleAPIEntity serviceTierType Vehicle {..} = VehicleAPIEntity {..}

data CreateRCInput = CreateRCInput
  { registrationNumber :: Maybe Text,
    fitnessUpto :: Maybe UTCTime,
    fleetOwnerId :: Maybe Text,
    vehicleCategory :: Maybe DVC.VehicleCategory,
    documentImageId :: Id Domain.Image,
    vehicleClass :: Maybe Text,
    vehicleClassCategory :: Maybe Text,
    insuranceValidity :: Maybe UTCTime,
    seatingCapacity :: Maybe Int,
    permitValidityUpto :: Maybe UTCTime,
    pucValidityUpto :: Maybe UTCTime,
    manufacturer :: Maybe Text,
    manufacturerModel :: Maybe Text,
    mYManufacturing :: Maybe Day,
    airConditioned :: Maybe Bool,
    oxygen :: Maybe Bool,
    ventilator :: Maybe Bool,
    bodyType :: Maybe Text,
    fuelType :: Maybe Text,
    color :: Maybe Text,
    dateOfRegistration :: Maybe UTCTime,
    vehicleModelYear :: Maybe Int,
    grossVehicleWeight :: Maybe Float,
    unladdenWeight :: Maybe Float
  }

getExpiryFailures :: DTC.TransporterConfig -> CreateRCInput -> UTCTime -> [Text]
getExpiryFailures transporterConfig input now =
  if transporterConfig.rcExpiryChecks == Just True
    then
      mapMaybe
        ( \(field, expiry) ->
            if maybe False (< now) expiry
              then Just (T.replace " " "" field <> "Expired")
              else Nothing
        )
        [ ("Fitness Certificate", input.fitnessUpto),
          ("Insurance", input.insuranceValidity),
          ("Permit", input.permitValidityUpto),
          ("PUC", input.pucValidityUpto)
        ]
    else []

buildRC :: VerificationFlow m r => Id DTM.Merchant -> Id DMOC.MerchantOperatingCity -> CreateRCInput -> [Text] -> m (Maybe VehicleRegistrationCertificate)
buildRC merchantId merchantOperatingCityId input failedRules = do
  now <- getCurrentTime
  id <- generateGUID
  rCConfigs <- getOneConfig (DocumentVerificationConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId, documentType = Just DVC.VehicleRegistrationCertificate, vehicleCategory = Just (fromMaybe DVC.CAR input.vehicleCategory)}) (Just (maybeToList <$> CQDVC.findByMerchantOpCityIdAndDocumentTypeAndCategory merchantOperatingCityId DVC.VehicleRegistrationCertificate (fromMaybe DVC.CAR input.vehicleCategory) Nothing)) >>= fromMaybeM (DocumentVerificationConfigNotFound merchantOperatingCityId.getId (show DVC.VehicleRegistrationCertificate))
  mEncryptedRC <- encrypt `mapM` input.registrationNumber
  let mbFitnessExpiry = input.fitnessUpto <|> input.permitValidityUpto <|> Just (UTCTime (TO.fromOrdinalDate 1900 1) 0)
  mbRC <- case (mEncryptedRC, mbFitnessExpiry) of
    (Just certificateNumber, Just expiry) -> do
      rc <- createRC merchantId merchantOperatingCityId input rCConfigs id now failedRules certificateNumber expiry
      logInfo $ "buildRC: Created RC with verificationStatus=" <> show rc.verificationStatus <> ", failedRules=" <> show failedRules <> ", registrationNumber=" <> show input.registrationNumber
      return $ Just rc
    _ -> return Nothing
  return mbRC

createRC ::
  VerificationFlow m r =>
  Id DTM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  CreateRCInput ->
  DVC.DocumentVerificationConfig ->
  Id VehicleRegistrationCertificate ->
  UTCTime ->
  [Text] ->
  EncryptedHashedField 'AsEncrypted Text ->
  UTCTime ->
  m VehicleRegistrationCertificate
createRC merchantId merchantOperatingCityId input rcconfigs id now failedRules certificateNumber expiry = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOperatingCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOperatingCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOperatingCityId.getId)
  (verificationStatus, reviewRequired, variant, mbVehicleModel) <- validateRCStatus input rcconfigs now expiry
  logInfo $ "createRC: verificationStatus=" <> show verificationStatus <> ", reviewRequired=" <> show reviewRequired <> ", variant=" <> show variant <> ", mbVehicleModel=" <> show mbVehicleModel
  let airConditioned = input.airConditioned
      updVariant = case DV.castVehicleVariantToVehicleCategory <$> variant of
        Just DVC.BUS -> if airConditioned == Just True then Just DV.BUS_AC else Just DV.BUS_NON_AC
        Just DVC.TRUCK -> Just $ DV.getTruckVehicleVariant input.grossVehicleWeight input.unladdenWeight (fromMaybe DV.DELIVERY_LIGHT_GOODS_VEHICLE variant)
        _ -> variant
      finalVerificationStatus = if null failedRules then verificationStatus else Documents.INVALID
  pure
    VehicleRegistrationCertificate
      { id,
        documentImageId = input.documentImageId,
        certificateNumber,
        fitnessExpiry = expiry,
        permitExpiry = input.permitValidityUpto,
        pucExpiry = input.pucValidityUpto,
        vehicleClass = input.vehicleClass,
        vehicleVariant = updVariant,
        vehicleManufacturer = input.manufacturer <|> input.manufacturerModel,
        vehicleCapacity = input.seatingCapacity,
        vehicleModel = mbVehicleModel,
        vehicleColor = input.color,
        vehicleDoors = Nothing,
        vehicleSeatBelts = Nothing,
        manufacturerModel = input.manufacturerModel,
        vehicleEnergyType = input.fuelType,
        reviewedAt = Nothing,
        reviewRequired,
        insuranceValidity = input.insuranceValidity,
        mYManufacturing = input.mYManufacturing,
        verificationStatus = finalVerificationStatus,
        fleetOwnerId = input.fleetOwnerId,
        merchantId = Just merchantId,
        merchantOperatingCityId = Just merchantOperatingCityId,
        userPassedVehicleCategory = input.vehicleCategory,
        airConditioned = airConditioned,
        oxygen = input.oxygen,
        ventilator = input.ventilator,
        luggageCapacity = Nothing,
        vehicleRating = Nothing,
        vehicleRatingRemark = Nothing,
        failedRules = failedRules,
        docsVerificationStatus =
          if transporterConfig.enableManualDocumentStatusCheck == Just True
            then Just DDVS.ADMIN_PENDING
            else Nothing,
        dateOfRegistration = input.dateOfRegistration,
        vehicleModelYear = input.vehicleModelYear,
        rejectReason = Nothing,
        createdAt = now,
        unencryptedCertificateNumber = input.registrationNumber,
        approved = Just False,
        updatedAt = now,
        vehicleImageId = Nothing,
        verified = Nothing,
        pendingChallan = Nothing
      }

validateRCStatus :: VerificationFlow m r => CreateRCInput -> DVC.DocumentVerificationConfig -> UTCTime -> UTCTime -> m (Documents.VerificationStatus, Maybe Bool, Maybe DV.VehicleVariant, Maybe Text)
validateRCStatus input rcconfigs now expiry = do
  case rcconfigs.supportedVehicleClasses of
    DVC.RCValidClasses [] -> pure (Documents.INVALID, Nothing, Nothing, Nothing)
    DVC.RCValidClasses vehicleClassVariantMap -> do
      let validCOVsCheck = rcconfigs.vehicleClassCheckType
      let mbVehicleClassOrCategory = input.vehicleClass <|> input.vehicleClassCategory
      logInfo $ "validateRCStatus: vehicleClass=" <> show input.vehicleClass <> ", vehicleClassCategory=" <> show input.vehicleClassCategory <> ", mbVehicleClassOrCategory=" <> show mbVehicleClassOrCategory <> ", vehicleClassCheckType=" <> show validCOVsCheck <> ", vehicleClassVariantMap size=" <> show (length vehicleClassVariantMap)
      let (isCOVValid, reviewRequired, variant, mbVehicleModel) = maybe (False, Nothing, Nothing, Nothing) (isValidCOVRC input.airConditioned input.oxygen input.ventilator input.vehicleClassCategory input.seatingCapacity input.manufacturer input.bodyType input.manufacturerModel vehicleClassVariantMap validCOVsCheck) mbVehicleClassOrCategory
      logDebug $ "validateRCStatus: reviewRequired=" <> show reviewRequired <> ", variant=" <> show variant <> ", mbVehicleModel=" <> show mbVehicleModel
      logInfo $ "validateRCStatus: isCOVValid=" <> show isCOVValid <> ", checkExpiry=" <> show rcconfigs.checkExpiry <> ", expiry=" <> show expiry <> ", now < expiry=" <> show (now < expiry)
      let validInsurance = True -- (not rcInsurenceConfigs.checkExpiry) || maybe False (now <) insuranceValidity
      let expiryCheck = (not rcconfigs.checkExpiry) || now < expiry
      let finalStatus = if expiryCheck && isCOVValid && validInsurance then Documents.VALID else Documents.INVALID
      logInfo $ "validateRCStatus: Setting verificationStatus=" <> show finalStatus <> " (expiryCheck=" <> show expiryCheck <> ", isCOVValid=" <> show isCOVValid <> ", validInsurance=" <> show validInsurance <> ")"
      pure $ if finalStatus == Documents.VALID then (Documents.VALID, reviewRequired, variant, mbVehicleModel) else (Documents.INVALID, reviewRequired, variant, mbVehicleModel)
    _ -> pure (Documents.INVALID, Nothing, Nothing, Nothing)

isValidCOVRC :: Maybe Bool -> Maybe Bool -> Maybe Bool -> Maybe Text -> Maybe Int -> Maybe Text -> Maybe Text -> Maybe Text -> [DVC.VehicleClassVariantMap] -> DVC.VehicleClassCheckType -> Text -> (Bool, Maybe Bool, Maybe DV.VehicleVariant, Maybe Text)
isValidCOVRC mbAirConditioned mbOxygen mbVentilator mVehicleCategory capacity manufacturer bodyType manufacturerModel vehicleClassVariantMap validCOVsCheck cov = do
  let sortedVariantMap = sortMaybe vehicleClassVariantMap
  let vehicleClassVariant = DL.find checkIfMatch sortedVariantMap
  case vehicleClassVariant of
    Just obj -> (True, obj.reviewRequired, Just obj.vehicleVariant, obj.vehicleModel)
    Nothing -> (False, Nothing, Nothing, Nothing)
  where
    checkIfMatch obj = do
      let classMatched = classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) (T.toUpper cov)
      let categoryMatched = maybe False (classCheckFunction validCOVsCheck (T.toUpper obj.vehicleClass) . T.toUpper) mVehicleCategory
      let capacityMatched = capacityCheckFunction obj.vehicleCapacity capacity
      let manufacturerMatched = manufacturerCheckFunction validCOVsCheck obj.manufacturer manufacturer
      let manufacturerModelMatched = manufacturerModelCheckFunction validCOVsCheck obj.manufacturerModel manufacturerModel
      let bodyTypeMatched = bodyTypeCheckFunction obj.bodyType bodyType
      let ambulanceMatched = if obj.vehicleVariant `elem` ambulanceVariants then checkAmbulanceVariant obj.vehicleVariant else ensureNonAmbulance bodyType manufacturerModel
      (classMatched || categoryMatched) && capacityMatched && manufacturerMatched && manufacturerModelMatched && bodyTypeMatched && ambulanceMatched

    ambulanceVariants = [DV.AMBULANCE_TAXI, DV.AMBULANCE_TAXI_OXY, DV.AMBULANCE_AC, DV.AMBULANCE_AC_OXY, DV.AMBULANCE_VENTILATOR] -- Todo: Create a fn to get variants by category
    checkAmbulanceVariant variant = case (mbAirConditioned, mbOxygen, mbVentilator) of
      (_, _, Just True) -> variant == DV.AMBULANCE_VENTILATOR
      (Just True, Just True, _) -> variant == DV.AMBULANCE_AC_OXY
      (Just True, _, _) -> variant == DV.AMBULANCE_AC
      (Just False, Just True, _) -> variant == DV.AMBULANCE_TAXI_OXY
      _ -> variant == DV.AMBULANCE_TAXI

    ensureNonAmbulance bodyType_ manufacturerModel_ = do
      let checkerLiteral = Just "AMBULANCE"
      case (bodyType_, manufacturerModel_) of
        (Nothing, Nothing) -> True
        (Just bt, Nothing) -> not $ bodyTypeCheckFunction checkerLiteral (Just bt)
        (Nothing, Just mfg) -> not $ manufacturerModelCheckFunction DVC.Infix checkerLiteral (Just mfg)
        (Just bt, Just mfg) -> not (bodyTypeCheckFunction checkerLiteral (Just bt) || manufacturerModelCheckFunction DVC.Infix checkerLiteral (Just mfg))

-- capacityCheckFunction validCapacity rcCapacity
capacityCheckFunction :: Maybe Int -> Maybe Int -> Bool
capacityCheckFunction (Just a) (Just b) = a == b
capacityCheckFunction Nothing (Just _) = True
capacityCheckFunction Nothing Nothing = True
capacityCheckFunction _ _ = False

manufacturerCheckFunction :: DVC.VehicleClassCheckType -> Maybe Text -> Maybe Text -> Bool
manufacturerCheckFunction validCOVsCheck (Just a) (Just b) = textFieldCheckFunction validCOVsCheck (T.toUpper a) (T.toUpper b)
manufacturerCheckFunction _ Nothing (Just _) = True
manufacturerCheckFunction _ Nothing Nothing = True
manufacturerCheckFunction _ _ _ = False

manufacturerModelCheckFunction :: DVC.VehicleClassCheckType -> Maybe Text -> Maybe Text -> Bool
manufacturerModelCheckFunction validCOVsCheck (Just a) (Just b) = textFieldCheckFunction validCOVsCheck (T.toUpper a) (T.toUpper b)
manufacturerModelCheckFunction _ Nothing (Just _) = True
manufacturerModelCheckFunction _ Nothing Nothing = True
manufacturerModelCheckFunction _ _ _ = False

bodyTypeCheckFunction :: Maybe Text -> Maybe Text -> Bool
bodyTypeCheckFunction (Just a) (Just b) = T.isInfixOf (T.toUpper a) (T.toUpper b)
bodyTypeCheckFunction Nothing (Just _) = True
bodyTypeCheckFunction Nothing Nothing = True
bodyTypeCheckFunction _ _ = False

textFieldCheckFunction :: DVC.VehicleClassCheckType -> Text -> Text -> Bool
textFieldCheckFunction validCOVsCheck =
  case validCOVsCheck of
    DVC.Exact -> (==)
    _ -> T.isInfixOf

classCheckFunction :: DVC.VehicleClassCheckType -> Text -> Text -> Bool
classCheckFunction validCOVsCheck =
  case validCOVsCheck of
    DVC.Infix -> T.isInfixOf
    DVC.Prefix -> T.isPrefixOf
    DVC.Suffix -> T.isSuffixOf
    DVC.Exact -> (==)

compareMaybe :: Ord a => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing Nothing = EQ
compareMaybe Nothing _ = GT
compareMaybe _ Nothing = LT
compareMaybe (Just x) (Just y) = compare x y

compareVehicles :: DVC.VehicleClassVariantMap -> DVC.VehicleClassVariantMap -> Ordering
compareVehicles a b =
  compareMaybe a.priority b.priority
    `mappend` compareMaybe a.manufacturer b.manufacturer
    `mappend` compareMaybe a.manufacturerModel b.manufacturerModel
    `mappend` compareMaybe a.vehicleCapacity b.vehicleCapacity

-- Function to sort list of Maybe values
sortMaybe :: [DVC.VehicleClassVariantMap] -> [DVC.VehicleClassVariantMap]
sortMaybe = DL.sortBy compareVehicles

removeSpaceAndDash :: Text -> Text
removeSpaceAndDash = T.replace "-" "" . T.replace " " ""

convertTextToDay :: Maybe Text -> Maybe Day
convertTextToDay a = do
  a_ <- a
  parseTimeM True defaultTimeLocale "%Y-%-m-%-d" $ T.unpack a_

convertUTCTimetoDate :: UTCTime -> Text
convertUTCTimetoDate utctime = T.pack (formatTime defaultTimeLocale "%d/%m/%Y" utctime)

-- | Parse a date string in "YYYY-MM-DD" format into a UTCTime (midnight UTC).
parseDateTime :: Text -> Maybe UTCTime
parseDateTime = parseTimeM True defaultTimeLocale "%Y-%m-%d" . T.unpack

data VerificationReqRecord = VerificationReqRecord
  { airConditioned :: Maybe Bool,
    docType :: DVC.DocumentType,
    documentImageId1 :: Id Domain.Image,
    documentImageId2 :: Maybe (Id Domain.Image),
    documentNumber :: EncryptedHashedField 'AsEncrypted Text,
    driverDateOfBirth :: Maybe UTCTime,
    driverId :: Id Person,
    verificaitonResponse :: Maybe Text,
    id :: Text,
    imageExtractionValidation :: DIdfy.ImageExtractionValidation,
    issueDateOnDoc :: Maybe UTCTime,
    nameOnCard :: Maybe Text,
    oxygen :: Maybe Bool,
    requestId :: Text,
    retryCount :: Maybe Int,
    status :: Text,
    vehicleCategory :: Maybe DVC.VehicleCategory,
    ventilator :: Maybe Bool,
    merchantId :: Maybe (Id DTM.Merchant),
    merchantOperatingCityId :: Maybe (Id DMOC.MerchantOperatingCity),
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving (Generic)

-- Nothing when the row's docType is not a DocumentType (e.g. synchronous face-compare audit rows).
makeIdfyVerificationReqRecord :: DIdfy.IdfyVerification -> Maybe VerificationReqRecord
makeIdfyVerificationReqRecord DIdfy.IdfyVerification {..} = do
  parsedDocType <- parseDocType docType
  Just $
    VerificationReqRecord
      { id = id.getId,
        verificaitonResponse = idfyResponse,
        docType = parsedDocType,
        ..
      }

makeHVVerificationReqRecord :: DHV.HyperVergeVerification -> VerificationReqRecord
makeHVVerificationReqRecord DHV.HyperVergeVerification {..} =
  VerificationReqRecord
    { id = id.getId,
      verificaitonResponse = hypervergeResponse,
      ..
    }

-- | Dedupe key for getTask pulls, per verification requestId — every pull path shares it, so a row is
--   pulled at most once per 'getTaskPullWindow'.
getTaskPullKey :: Text -> Text
getTaskPullKey requestId = "verifyPull:req:" <> requestId

getTaskPullWindow :: NominalDiffTime
getTaskPullWindow = 30

-- | Fixed-window cap: at most 'getTaskAttemptLimit' getTask pulls per requestId per
--   'getTaskAttemptWindow'; on hitting it, pulls pause until the window expires — never a permanent
--   stop. 10 covers ~5 min of the sync DL backstop's tightest (30s-deduped) polling.
allowGetTaskAttempt :: (CacheFlow m r) => Text -> m Bool
allowGetTaskAttempt requestId = do
  let key = "verifyPull:attempts:" <> requestId
  -- SET NX EX before INCR: the TTL is established atomically with key creation, so a crash between
  -- the two calls can never leave a counter without expiry (= this requestId blocked forever).
  void $ Redis.setNxExpire key getTaskAttemptWindow (0 :: Int)
  attempts <- Redis.incr key
  pure (attempts <= getTaskAttemptLimit)

getTaskAttemptLimit :: Integer
getTaskAttemptLimit = 10

getTaskAttemptWindow :: Int
getTaskAttemptWindow = 7200 -- 2 hours

toMaybe :: [a] -> Kernel.Prelude.Maybe [a]
toMaybe [] = Kernel.Prelude.Nothing
toMaybe xs = Kernel.Prelude.Just xs

filterVehicleDocuments :: [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig] -> Maybe Bool -> [Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig]
filterVehicleDocuments docs onlyVehicle =
  if onlyVehicle == Just True
    then filter (\Domain.Types.DocumentVerificationConfig.DocumentVerificationConfig {..} -> documentType `elem` defaultVehicleDocumentTypes) docs
    else docs

filterInCompatibleFlows ::
  HasField "filterForOldApks" documentVerificationConfigAPIEntity (Maybe Bool) =>
  Maybe Bool ->
  [documentVerificationConfigAPIEntity] ->
  [documentVerificationConfigAPIEntity]
filterInCompatibleFlows makeSelfieAadhaarPanMandatory = filter (\doc -> not (fromMaybe False doc.filterForOldApks) || fromMaybe False makeSelfieAadhaarPanMandatory)

mkFleetOwnerDocumentVerificationConfigAPIEntity :: Language -> Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig -> Environment.Flow API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigAPIEntity
mkFleetOwnerDocumentVerificationConfigAPIEntity language Domain.Types.FleetOwnerDocumentVerificationConfig.FleetOwnerDocumentVerificationConfig {..} = do
  mbTitle <- getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOperatingCityId.getId, messageKey = show documentType <> "_Title", language = Just language}) (Just (MTQuery.findByErrorAndLanguage (show documentType <> "_Title") language))
  mbDescription <- getConfig (TranslationDimensions {merchantOperatingCityId = Just merchantOperatingCityId.getId, messageKey = show documentType <> "_Description", language = Just language}) (Just (MTQuery.findByErrorAndLanguage (show documentType <> "_Description") language))
  return $
    API.Types.ProviderPlatform.Fleet.Onboarding.DocumentVerificationConfigAPIEntity
      { title = maybe title (.message) mbTitle,
        description = maybe description (Just . (.message)) mbDescription,
        applicableTo = API.Types.ProviderPlatform.Fleet.Onboarding.FLEET,
        filterForOldApks = Nothing,
        rcNumberPrefixList = [],
        documentType = castDocumentType documentType,
        dependencyDocumentType = map castDocumentType dependencyDocumentType,
        documentCategory = castDocumentCategory <$> documentCategory,
        isMandatoryForEnabling = fromMaybe isMandatory isMandatoryForEnabling,
        documentFields = Nothing,
        documentFlowGrouping = castDocumentFlowGrouping DVC.STANDARD,
        isReminderSupported = Nothing,
        isApprovalSupported = Nothing,
        rolesAllowedToUploadDocument = fmap (mapMaybe castPersonRoleToDashboardAccessType) rolesAllowedToUploadDocument,
        ..
      }

castPersonRoleToDashboardAccessType :: Role -> Maybe API.Types.ProviderPlatform.Management.Endpoints.Account.DashboardAccessType
castPersonRoleToDashboardAccessType FLEET_OWNER = Just API.Types.ProviderPlatform.Management.Endpoints.Account.FLEET_OWNER
castPersonRoleToDashboardAccessType FLEET_BUSINESS = Just API.Types.ProviderPlatform.Management.Endpoints.Account.FLEET_OWNER
castPersonRoleToDashboardAccessType ADMIN = Just API.Types.ProviderPlatform.Management.Endpoints.Account.DASHBOARD_ADMIN
castPersonRoleToDashboardAccessType OPERATOR = Just API.Types.ProviderPlatform.Management.Endpoints.Account.DASHBOARD_OPERATOR
castPersonRoleToDashboardAccessType _ = Nothing

castDocumentApplicableType :: Domain.Types.DocumentVerificationConfig.DocumentApplicableType -> API.Types.ProviderPlatform.Fleet.Onboarding.DocumentApplicableType
castDocumentApplicableType = \case
  Domain.Types.DocumentVerificationConfig.FLEET -> API.Types.ProviderPlatform.Fleet.Onboarding.FLEET
  Domain.Types.DocumentVerificationConfig.INDIVIDUAL -> API.Types.ProviderPlatform.Fleet.Onboarding.INDIVIDUAL
  Domain.Types.DocumentVerificationConfig.FLEET_AND_INDIVIDUAL -> API.Types.ProviderPlatform.Fleet.Onboarding.FLEET_AND_INDIVIDUAL

castDocumentFlowGrouping :: Domain.Types.DocumentVerificationConfig.DocumentFlowGrouping -> API.Types.ProviderPlatform.Fleet.Onboarding.DocumentFlowGrouping
castDocumentFlowGrouping = \case
  Domain.Types.DocumentVerificationConfig.COMMON -> API.Types.ProviderPlatform.Fleet.Onboarding.COMMON
  Domain.Types.DocumentVerificationConfig.STANDARD -> API.Types.ProviderPlatform.Fleet.Onboarding.STANDARD

castDocumentFieldInfo :: Domain.Types.DocumentVerificationConfig.FieldInfo -> API.Types.ProviderPlatform.Fleet.Onboarding.FieldInfo
castDocumentFieldInfo Domain.Types.DocumentVerificationConfig.FieldInfo {..} =
  API.Types.ProviderPlatform.Fleet.Onboarding.FieldInfo
    { name = name,
      _type = castDocumentFieldType _type,
      isMandatory = isMandatory,
      regexValidation = regexValidation
    }

castDocumentFieldType :: Domain.Types.DocumentVerificationConfig.FieldType -> API.Types.ProviderPlatform.Fleet.Onboarding.FieldType
castDocumentFieldType = \case
  Domain.Types.DocumentVerificationConfig.FieldText -> API.Types.ProviderPlatform.Fleet.Onboarding.FieldText
  Domain.Types.DocumentVerificationConfig.FieldInt -> API.Types.ProviderPlatform.Fleet.Onboarding.FieldInt
  Domain.Types.DocumentVerificationConfig.FieldDouble -> API.Types.ProviderPlatform.Fleet.Onboarding.FieldDouble

castDocumentCategory :: Domain.Types.DocumentVerificationConfig.DocumentCategory -> API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.DocumentCategory
castDocumentCategory = \case
  Domain.Types.DocumentVerificationConfig.Driver -> API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.Driver
  Domain.Types.DocumentVerificationConfig.Vehicle -> API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.Vehicle
  Domain.Types.DocumentVerificationConfig.Permission -> API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.Permission
  Domain.Types.DocumentVerificationConfig.Training -> API.Types.ProviderPlatform.Fleet.Endpoints.Onboarding.Training

castDocumentType :: Domain.Types.DocumentVerificationConfig.DocumentType -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DocumentType
castDocumentType = \case
  Domain.Types.DocumentVerificationConfig.DriverLicense -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DriverLicense
  Domain.Types.DocumentVerificationConfig.VehicleRegistrationCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleRegistrationCertificate
  Domain.Types.DocumentVerificationConfig.Permissions -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.Permissions
  Domain.Types.DocumentVerificationConfig.SubscriptionPlan -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.SubscriptionPlan
  Domain.Types.DocumentVerificationConfig.ProfilePhoto -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.ProfilePhotoImage
  Domain.Types.DocumentVerificationConfig.AadhaarCard -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.AadhaarCard
  Domain.Types.DocumentVerificationConfig.PanCard -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.PanCard
  Domain.Types.DocumentVerificationConfig.VehiclePermit -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehiclePermitImage
  Domain.Types.DocumentVerificationConfig.VehicleFitnessCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleFitnessCertificateImage
  Domain.Types.DocumentVerificationConfig.VehicleInsurance -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleInsuranceImage
  Domain.Types.DocumentVerificationConfig.VehiclePUC -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehiclePUCImage
  Domain.Types.DocumentVerificationConfig.ProfileDetails -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.ProfileDetails
  Domain.Types.DocumentVerificationConfig.SocialSecurityNumber -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.SocialSecurityNumber
  Domain.Types.DocumentVerificationConfig.VehicleInspectionForm -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleInspectionImage
  Domain.Types.DocumentVerificationConfig.DriverInspectionForm -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DriverInspectionFormImage
  Domain.Types.DocumentVerificationConfig.TrainingForm -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TrainingFormImage
  Domain.Types.DocumentVerificationConfig.GSTCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.GSTCertificate
  Domain.Types.DocumentVerificationConfig.BackgroundVerification -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.BackgroundVerification
  Domain.Types.DocumentVerificationConfig.UploadProfile -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.UploadProfileImage
  Domain.Types.DocumentVerificationConfig.VehicleNOC -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleNOC
  Domain.Types.DocumentVerificationConfig.DriverVehicleNOC -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DriverVehicleNOC
  Domain.Types.DocumentVerificationConfig.BusinessLicense -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.BusinessLicense
  Domain.Types.DocumentVerificationConfig.VehicleFront -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleFront
  Domain.Types.DocumentVerificationConfig.VehicleBack -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleBack
  Domain.Types.DocumentVerificationConfig.VehicleRight -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleRight
  Domain.Types.DocumentVerificationConfig.VehicleLeft -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleLeft
  Domain.Types.DocumentVerificationConfig.VehicleFrontInterior -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleFrontInterior
  Domain.Types.DocumentVerificationConfig.VehicleBackInterior -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleBackInterior
  Domain.Types.DocumentVerificationConfig.Odometer -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.Odometer
  Domain.Types.DocumentVerificationConfig.InspectionHub -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.InspectionHub
  Domain.Types.DocumentVerificationConfig.DriverInspectionHub -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DriverInspectionHub
  -- Netherlands Document Types
  Domain.Types.DocumentVerificationConfig.KIWADriverCard -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.KIWADriverCard
  Domain.Types.DocumentVerificationConfig.KIWATaxiPermit -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.KIWATaxiPermit
  Domain.Types.DocumentVerificationConfig.KvKChamberOfCommerceRegistration -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.KvKChamberOfCommerceRegistration
  Domain.Types.DocumentVerificationConfig.TAXDetails -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TAXDetails
  Domain.Types.DocumentVerificationConfig.BankingDetails -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.BankingDetails
  Domain.Types.DocumentVerificationConfig.VehicleDetails -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VehicleDetails
  Domain.Types.DocumentVerificationConfig.SchipolAirportAgreement -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.SchipolAirportAgreement
  Domain.Types.DocumentVerificationConfig.SchipolSmartcardProof -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.SchipolSmartcardProof
  Domain.Types.DocumentVerificationConfig.TXQualityMark -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TXQualityMark
  -- Finland Document Types
  Domain.Types.DocumentVerificationConfig.TaxiDriverPermit -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TaxiDriverPermit
  Domain.Types.DocumentVerificationConfig.TaxiTransportLicense -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TaxiTransportLicense
  Domain.Types.DocumentVerificationConfig.FinnishIDResidencePermit -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.FinnishIDResidencePermit
  Domain.Types.DocumentVerificationConfig.BusinessRegistrationExtract -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.BusinessRegistrationExtract
  Domain.Types.DocumentVerificationConfig.PersonalId -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.PersonalId
  Domain.Types.DocumentVerificationConfig.LocalResidenceProof -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.LocalResidenceProof
  Domain.Types.DocumentVerificationConfig.PoliceVerificationCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.PoliceVerificationCertificate
  Domain.Types.DocumentVerificationConfig.DrivingSchoolCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.DrivingSchoolCertificate
  -- India Certificate Document Types
  Domain.Types.DocumentVerificationConfig.LDCCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.LDCCertificate
  Domain.Types.DocumentVerificationConfig.TDSCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TDSCertificate
  Domain.Types.DocumentVerificationConfig.TANCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.TANCertificate
  Domain.Types.DocumentVerificationConfig.UDYAMCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.UDYAMCertificate
  Domain.Types.DocumentVerificationConfig.PanAadhaarLinkage -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.PanAadhaarLink
  Domain.Types.DocumentVerificationConfig.VoterIdCard -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.VoterIdCard
  Domain.Types.DocumentVerificationConfig.OperatorPartnerCode -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.OperatorPartnerCode
  Domain.Types.DocumentVerificationConfig.MedicalCertificate -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.MedicalCertificate
  Domain.Types.DocumentVerificationConfig.Rating -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.Rating
  Domain.Types.DocumentVerificationConfig.BotApproval -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.BotApproval
  Domain.Types.DocumentVerificationConfig.NomineeDetails -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.NomineeDetails
  Domain.Types.DocumentVerificationConfig.FleetRegistration -> API.Types.ProviderPlatform.Management.Endpoints.DriverRegistration.FleetRegistration

-- Shared document-onboarding helpers (moved from Domain.Action.UI.DriverOnboarding.VehicleRegistrationCertificate):
-- these are used across RC, PAN, Aadhaar, DL, GST and Idfy webhook flows.

data DriverDocument = DriverDocument
  { panNumber :: Maybe Text,
    aadhaarNumber :: Maybe Text,
    dlNumber :: Maybe Text,
    gstNumber :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON)

getRegexRulesFromDocumentConfig :: ODC.DocumentVerificationConfig -> [Text]
getRegexRulesFromDocumentConfig config = maybe [] (mapMaybe (.regexValidation)) config.documentFields

matchesRegexSafely :: Text -> Text -> Text -> Flow (Maybe Bool)
matchesRegexSafely documentType input regexPattern = do
  result <- try @_ @SomeException $ do
    let matched = (T.unpack input =~ T.unpack regexPattern :: Bool)
    matched `seq` pure matched
  case result of
    Right matched -> pure (Just matched)
    Left err -> do
      logError $ "Invalid regex in DocumentVerificationConfig for " <> documentType <> " validation: " <> regexPattern <> ", error: " <> show err
      pure Nothing

validateByRegex :: Text -> ODC.DocumentVerificationConfig -> Text -> Flow Bool -> Flow Bool
validateByRegex documentType config input fallback = do
  let regexRules = getRegexRulesFromDocumentConfig config
  if null regexRules
    then fallback
    else do
      regexResults <- mapM (matchesRegexSafely documentType input) regexRules
      let validRegexResults = mapMaybe (\x -> x) regexResults
      if null validRegexResults
        then fallback
        else pure (or validRegexResults)

imageS3Lock :: Text -> Text
imageS3Lock path = "image-s3-lock-" <> path

isNameComparePercentageValid :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Verification.NameCompareReq -> Flow Bool
isNameComparePercentageValid merchantId merchantOpCityId req = do
  transporterConfig <- getOneConfig (TransporterConfigDimensions {merchantOperatingCityId = merchantOpCityId.getId}) (Just (SCTC.findByMerchantOpCityId merchantOpCityId Nothing)) >>= fromMaybeM (TransporterConfigNotFound merchantOpCityId.getId)
  case transporterConfig.validNameComparePercentage of
    Just percentage -> do
      resp <- Verification.nameCompare merchantId merchantOpCityId req
      logDebug $ "Name compare percentage response: " <> show resp
      case resp.nameComparedData of
        Just percentageData -> return $ percentageData.match_output.name_match >= percentage
        Nothing -> throwError $ InternalError "Name comparison service returned invalid response"
    Nothing -> return True -- If percentage not configured, assume valid

getValidDocumentImage :: Id Person.Person -> Text -> ODC.DocumentType -> Flow Text
getValidDocumentImage personId imageId_ expectedDocType = do
  imageMetadata <- ImageQuery.findById (Id imageId_) >>= fromMaybeM (ImageNotFound imageId_)
  unless (imageMetadata.verificationStatus == Just Documents.VALID) $
    throwError (ImageNotValid imageId_)
  unless (imageMetadata.personId == personId) $
    throwError (ImageNotBelongsToPerson imageId_)
  unless (imageMetadata.imageType == expectedDocType) $
    throwError (ImageInvalidType (show expectedDocType) "")
  getImageFromS3 imageMetadata

getImageFromS3 :: Image.Image -> Flow Text
getImageFromS3 imageMetadata =
  Redis.withLockRedisAndReturnValue (imageS3Lock imageMetadata.s3Path) 5 $
    S3.get $ T.unpack imageMetadata.s3Path

-- | Outcome of a server-side selfie-vs-document face match.
--   FMSkip     -> face match not required, or document already SDK-matched on-device.
--   FMPass     -> faces matched.
--   FMFail     -> faces did not match; the document Image has been marked INVALID.
--   FMDeferred -> doc image not VALID yet, or no source (selfie) image available yet; resolve later.
data FaceMatchOutcome = FMSkip | FMPass | FMFail | FMDeferred
  deriving (Eq, Show)

-- | Sync Idfy face compare, memoized+audited: reuse a "completed" faceCompare* row for this image, else call Idfy and persist the outcome (success or failure) as one.
recordedFaceCompare ::
  Person.Person ->
  ODC.DocumentType ->
  Id Image.Image ->
  Id Image.Image ->
  Text ->
  Text ->
  Maybe Text ->
  Flow Bool
recordedFaceCompare person docType docImageId selfieImageId documentImage1 documentImage2 mbPlainDocNumber = do
  docTag <- faceCompareDocTag docType & fromMaybeM (InternalError $ "Face compare not supported for docType: " <> show docType)
  mbExistingRow <- listToMaybe <$> IVQuery.findLatestByDocTypeAndDocumentImageId1 (Just 1) Nothing person.id docTag docImageId
  case mbExistingRow >>= reusableResult of
    Just matched -> do
      logInfo $ "Reusing recorded face compare result for image " <> docImageId.getId <> " (" <> docTag <> "): matched=" <> show matched
      pure matched
    Nothing -> do
      eresp <-
        try @_ @SomeException $
          Verification.faceCompare person.merchantId person.merchantOperatingCityId $
            VI.FaceCompareReq {documentImage1, documentImage2, driverId = person.id.getId}
      encDocNumber <- resolveDocNumberEnc
      now <- getCurrentTime
      rowId <- generateGUID
      requestId <- generateGUID
      let mkAuditRow status mbResp =
            DIdfy.IdfyVerification
              { id = rowId,
                driverId = person.id,
                documentImageId1 = docImageId,
                documentImageId2 = Just selfieImageId,
                requestId,
                docType = docTag,
                documentNumber = encDocNumber,
                driverDateOfBirth = Nothing,
                imageExtractionValidation = DIdfy.Skipped,
                issueDateOnDoc = Nothing,
                status,
                idfyResponse = mbResp,
                vehicleCategory = Nothing,
                airConditioned = Nothing,
                oxygen = Nothing,
                ventilator = Nothing,
                retryCount = Just 0,
                nameOnCard = Nothing,
                merchantId = Just person.merchantId,
                merchantOperatingCityId = Just person.merchantOperatingCityId,
                createdAt = now,
                updatedAt = now
              }
      case eresp of
        Left err -> do
          IVQuery.create $ mkAuditRow "failed" (Just $ show err)
          throwError $ InternalError $ "Face compare API call failed for driver " <> person.id.getId <> ": " <> show err
        Right resp -> do
          IVQuery.create $ mkAuditRow "completed" (Just $ encodeToText resp)
          pure $ (resp.faceComparedData >>= (.is_a_match)) == Just True
  where
    reusableResult :: DIdfy.IdfyVerification -> Maybe Bool
    reusableResult row
      | row.status /= "completed" = Nothing
      | otherwise = do
        respTxt <- row.idfyResponse
        (resp :: VI.FaceCompareResp) <- decodeFromText respTxt
        Just $ (resp.faceComparedData >>= (.is_a_match)) == Just True
    resolveDocNumberEnc = case mbPlainDocNumber of
      Just plain -> encrypt plain
      Nothing -> do
        mbEnc <- case docType of
          ODC.DriverLicense -> fmap (.licenseNumber) <$> DLQuery.findByDriverId person.id
          ODC.PanCard -> fmap (.panCardNumber) <$> DPQuery.findByDriverId person.id
          ODC.AadhaarCard -> (>>= (.aadhaarNumber)) <$> QAadhaarCard.findByPrimaryKey person.id
          _ -> pure Nothing
        -- documentNumber is mandatory; the imageId is a greppable non-sensitive sentinel when no number exists yet.
        maybe (encrypt docImageId.getId) pure mbEnc

-- | Face-match a doc image against the configured source doc (selfie); DRIVER-only, DL/PAN/Aadhaar-only, non-SDK-only; a mismatch marks the image INVALID with FaceMatchFailed.
runDocFaceMatch :: Person.Person -> ODC.DocumentVerificationConfig -> Id Image.Image -> Maybe Text -> Maybe Text -> Flow FaceMatchOutcome
runDocFaceMatch person config docImageId mbDocImage mbPlainDocNumber
  | person.role /= Person.DRIVER = pure FMSkip
  | isNothing (faceCompareDocTag config.documentType) = pure FMSkip
  | otherwise =
    case config.faceMatchSourceDoc of
      Nothing -> pure FMSkip
      Just sourceDocType -> do
        docImage <- ImageQuery.findById docImageId >>= fromMaybeM (ImageNotFound docImageId.getId)
        -- SDK-matched or already-INVALID -> skip; not-yet-VALID -> defer until the image becomes VALID.
        if isJust docImage.workflowTransactionId
          then pure FMSkip
          else case docImage.verificationStatus of
            Just Documents.INVALID -> pure FMSkip
            Just Documents.VALID -> do
              sourceImages <- ImageQuery.findRecentByPersonIdAndImageType person.id sourceDocType
              case DL.find ((== Just Documents.VALID) . (.verificationStatus)) sourceImages of
                Nothing -> pure FMDeferred -- no selfie yet; resolve when it arrives
                Just sourceImage -> do
                  documentImage1 <- maybe (getImageFromS3 docImage) pure mbDocImage
                  documentImage2 <- getValidDocumentImage person.id sourceImage.id.getId sourceDocType
                  matched <- recordedFaceCompare person config.documentType docImageId sourceImage.id documentImage1 documentImage2 mbPlainDocNumber
                  if matched
                    then pure FMPass
                    else do
                      ImageQuery.updateVerificationStatusAndFailureReason Documents.INVALID FaceMatchFailed docImageId
                      logInfo $ "Face match failed for document " <> show config.documentType <> " of driver " <> person.id.getId
                      pure FMFail
            _ -> pure FMDeferred -- PENDING / MANUAL_VERIFICATION_REQUIRED: resolve when image becomes VALID

-- | Status to write where a flow would write VALID: FMPass/FMSkip -> VALID, FMFail -> INVALID, FMDeferred (no selfie yet) -> PENDING.
resolveFaceMatchVerificationStatus :: Person.Person -> ODC.DocumentVerificationConfig -> Id Image.Image -> Maybe Text -> Maybe Text -> Flow Documents.VerificationStatus
resolveFaceMatchVerificationStatus person config docImageId mbDocImage mbPlainDocNumber = do
  outcome <- runDocFaceMatch person config docImageId mbDocImage mbPlainDocNumber
  pure $ case outcome of
    FMFail -> Documents.INVALID
    FMDeferred -> Documents.PENDING
    _ -> Documents.VALID

-- | On selfie upload, face-match earlier non-SDK doc images: skip while the doc's webhook is pending (the handler resolves it), else run now and promote PENDING->VALID or flip INVALID.
faceMatchBoundConfigs :: Person.Person -> Flow [ODC.DocumentVerificationConfig]
faceMatchBoundConfigs person =
  DL.nubBy (\a b -> a.documentType == b.documentType) . filter (\c -> isJust c.faceMatchSourceDoc && isJust (faceCompareDocTag c.documentType)) <$> CQDVC.findAllByMerchantOpCityId person.merchantOperatingCityId Nothing

runDeferredFaceMatchOnSelfie :: Person.Person -> UTCTime -> Flow ()
runDeferredFaceMatchOnSelfie person selfieCreatedAt = do
  faceMatchConfigs <- faceMatchBoundConfigs person
  forM_ faceMatchConfigs $ \config -> do
    result <- try @_ @SomeException $ matchDeferredDoc config
    case result of
      Right _ -> pure ()
      Left err -> logError $ "Deferred face match failed for " <> show config.documentType <> " of driver " <> person.id.getId <> ": " <> show err
  where
    matchDeferredDoc config = do
      let docType = config.documentType
      docImages <- ImageQuery.findRecentByPersonIdAndImageType person.id docType
      whenJust (DL.find (\img -> img.verificationStatus /= Just Documents.INVALID && isNothing img.workflowTransactionId && img.createdAt < selfieCreatedAt) docImages) $ \docImg -> do
        mbVerificationRow <- listToMaybe <$> IVQuery.findLatestByDocTypeAndDocumentImageId1 (Just 1) Nothing person.id (docTypeToText docType) docImg.id
        let webhookPending = maybe False ((`elem` inProgressIdfyStatuses) . (.status)) mbVerificationRow
        if webhookPending
          then logInfo $ "Deferred face match: webhook still in flight for " <> show docType <> " image " <> docImg.id.getId <> "; the webhook handler will resolve it."
          else do
            outcome <- runDocFaceMatch person config docImg.id Nothing Nothing
            case outcome of
              FMFail -> case docType of
                ODC.DriverLicense -> DLQuery.updateVerificationStatus Documents.INVALID docImg.id
                ODC.PanCard -> DPQuery.updateVerificationStatus Documents.INVALID person.id
                ODC.AadhaarCard -> QAadhaarCard.updateVerificationStatus Documents.INVALID person.id
                _ -> pure ()
              FMPass -> promotePendingToValid docType docImg
              _ -> pure ()
    -- Promote only PENDING records belonging to this exact image; legacy VALID rows are never touched.
    promotePendingToValid docType docImg = case docType of
      ODC.DriverLicense -> do
        mbDL <- DLQuery.findByDriverId person.id
        whenJust mbDL $ \dl ->
          when (dl.documentImageId1 == docImg.id && dl.verificationStatus == Documents.PENDING) $
            DLQuery.updateVerificationStatus Documents.VALID docImg.id
      ODC.PanCard -> do
        mbPan <- DPQuery.findByDriverId person.id
        whenJust mbPan $ \pan ->
          when (pan.documentImageId1 == docImg.id && pan.verificationStatus == Documents.PENDING) $
            DPQuery.updateVerificationStatus Documents.VALID person.id
      ODC.AadhaarCard -> do
        mbAadhaar <- QAadhaarCard.findByPrimaryKey person.id
        whenJust mbAadhaar $ \aadhaar ->
          when (aadhaar.aadhaarFrontImageId == Just docImg.id && aadhaar.verificationStatus == Documents.PENDING) $
            QAadhaarCard.updateVerificationStatus Documents.VALID person.id
      _ -> pure ()

-- | Once a VALID selfie exists, a DRIVER may upload a new one only while every face-match-bound doc (DL/PAN/Aadhaar with faceMatchSourceDoc set) is absent or INVALID.
enforceSelfieReuploadPolicy :: Person.Person -> [Image.Image] -> Flow ()
enforceSelfieReuploadPolicy person priorSelfies =
  when (person.role == Person.DRIVER && any ((== Just Documents.VALID) . (.verificationStatus)) priorSelfies) $ do
    faceMatchDocTypes <- fmap (.documentType) <$> faceMatchBoundConfigs person
    docStatuses <- catMaybes <$> mapM lookupDocStatus faceMatchDocTypes
    let blockingDocs = filter ((/= Documents.INVALID) . snd) docStatuses
    whenJust (pickBlockingDoc blockingDocs) $ \(docType, status) -> do
      logInfo $ "Selfie re-upload blocked for driver " <> person.id.getId <> "; non-INVALID face-match docs: " <> show blockingDocs
      throwError $ SelfieReuploadNotAllowed (show docType) status
  where
    lookupDocStatus docType = case docType of
      ODC.DriverLicense -> fmap (\dl -> (docType, dl.verificationStatus)) <$> DLQuery.findByDriverId person.id
      ODC.PanCard -> fmap (\pan -> (docType, pan.verificationStatus)) <$> DPQuery.findByDriverId person.id
      ODC.AadhaarCard -> fmap (\aadhaar -> (docType, aadhaar.verificationStatus)) <$> QAadhaarCard.findByPrimaryKey person.id
      _ -> pure Nothing
    -- Highest-severity blocker names the error: VALID > MANUAL_VERIFICATION_REQUIRED > in-flight (PENDING/etc.).
    pickBlockingDoc docs =
      DL.find ((== Documents.VALID) . snd) docs
        <|> DL.find ((== Documents.MANUAL_VERIFICATION_REQUIRED) . snd) docs
        <|> listToMaybe docs

mkRCIdfyVerificationEntity :: MonadFlow m => Person.Person -> Text -> UTCTime -> DIdfy.ImageExtractionValidation -> EncryptedHashedField 'AsEncrypted Text -> Maybe UTCTime -> Maybe DVC.VehicleCategory -> Maybe Bool -> Maybe Bool -> Maybe Bool -> Id Image.Image -> Maybe Int -> Maybe Text -> m DIdfy.IdfyVerification
mkRCIdfyVerificationEntity person requestId now imageExtractionValidation encryptedRC dateOfRegistration mbVehicleCategory mbAirConditioned mbOxygen mbVentilator imageId mbRetryCnt mbStatus = do
  id <- generateGUID
  return $
    DIdfy.IdfyVerification
      { id,
        driverId = person.id,
        documentImageId1 = imageId,
        documentImageId2 = Nothing,
        requestId,
        docType = docTypeToText ODC.VehicleRegistrationCertificate,
        documentNumber = encryptedRC,
        driverDateOfBirth = Nothing,
        imageExtractionValidation = imageExtractionValidation,
        issueDateOnDoc = dateOfRegistration,
        status = fromMaybe "pending" mbStatus,
        idfyResponse = Nothing,
        vehicleCategory = mbVehicleCategory,
        airConditioned = mbAirConditioned,
        oxygen = mbOxygen,
        ventilator = mbVentilator,
        retryCount = Just $ fromMaybe 0 mbRetryCnt,
        nameOnCard = Nothing,
        merchantId = Just person.merchantId,
        merchantOperatingCityId = Just person.merchantOperatingCityId,
        createdAt = now,
        updatedAt = now
      }

compareNames :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Id Person.Person -> Flow Bool
compareNames merchantId merchantOpCityId mbExtractedName mbVerifiedName personId =
  case (mbExtractedName, mbVerifiedName) of
    (Just extractedName, Just verifiedName) -> do
      let nameCompareReq =
            Verification.NameCompareReq
              { extractedName = extractedName,
                verifiedName = verifiedName,
                percentage = Just True,
                driverId = personId.getId
              }
      isNameValid <- isNameComparePercentageValid merchantId merchantOpCityId nameCompareReq
      unless isNameValid $ throwError (MismatchDataError "Name match failed with previously uploaded docs")
      return True
    _ -> do
      logInfo "Name comparison checks not executed."
      return False

compareDateOfBirth :: Maybe UTCTime -> Maybe UTCTime -> Flow Bool
compareDateOfBirth mbExtractedValue mbVerifiedValue = do
  case (mbExtractedValue, mbVerifiedValue) of
    (Just extractedValue, Just verifiedValue) -> do
      let extractedDay = utctDay extractedValue
          verifiedDay = utctDay verifiedValue
      unless (extractedDay == verifiedDay) $ throwError (MismatchDataError $ "Date of birth mismatch: " <> show extractedDay <> " " <> show verifiedDay)
      return True
    _ -> do
      logInfo "Date of birth checks not executed."
      return False

checkPan :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkPan merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  mdriverPanInformation <- DPQuery.findByDriverId personId
  case mdriverPanInformation of
    Just panData -> do
      if verifyingDocumentType `elem` [ODC.AadhaarCard, ODC.DriverLicense]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue panData.driverNameOnGovtDB mbDateOfBirthValue panData.driverDob personId
        else return False
    Nothing -> throwError $ InternalError "Pan not found"

checkDL :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkDL merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  mdriverLicense <- DLQuery.findByDriverId personId
  case mdriverLicense of
    Just dlData -> do
      if verifyingDocumentType `elem` [ODC.AadhaarCard, ODC.PanCard]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue dlData.driverName mbDateOfBirthValue dlData.driverDob personId
        else return False
    Nothing -> throwError $ InternalError "DL not found"

checkAadhaar :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe UTCTime -> ODC.DocumentType -> Flow Bool
checkAadhaar merchantId merchantOpCityId personId mbExtractedValue mbDateOfBirthValue verifyingDocumentType = do
  aadhaarInfo <- QAadhaarCard.findByPrimaryKey personId
  case aadhaarInfo of
    Just aadhaarData -> do
      if verifyingDocumentType `elem` [ODC.PanCard, ODC.DriverLicense]
        then validateNameAndDOB merchantId merchantOpCityId mbExtractedValue aadhaarData.nameOnCard mbDateOfBirthValue (parseDateTime =<< aadhaarData.dateOfBirth) personId
        else return False
    Nothing -> throwError $ InternalError "Aadhaar not found"

checkGST :: Id Person.Person -> Maybe Text -> Flow ()
checkGST personId mbPanNumber = do
  mGstData <- DGQuery.findByDriverId personId
  case mGstData of
    Just gstData -> checkTwoPanNumber mbPanNumber gstData.panNumber
    Nothing -> throwError $ InternalError "GST not found"

validateDocument :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id Person.Person -> Maybe Text -> Maybe Text -> Maybe Text -> ODC.DocumentType -> DriverDocument -> Flow ()
validateDocument merchantId merchantOpCityId personId mbNameValue mbDateOfBirthValue mbPanNumber verifyingDocumentType DriverDocument {..} = do
  let mbUtcDateOfBirthValue = parseDateTime =<< mbDateOfBirthValue
  let skipPanAadhaarCrossCheck = isBusinessPan panNumber || isBusinessPan mbPanNumber
  case verifyingDocumentType of
    ODC.AadhaarCard -> do
      panChecked <- case panNumber of
        Just _
          | not skipPanAadhaarCrossCheck ->
            checkPan merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.AadhaarCard
        _ -> pure False
      unless panChecked $
        whenJust dlNumber $ \_ ->
          void $ checkDL merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.AadhaarCard
    ODC.PanCard -> do
      aadhaarChecked <- case aadhaarNumber of
        Just _
          | not skipPanAadhaarCrossCheck ->
            checkAadhaar merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.PanCard
        _ -> pure False
      unless aadhaarChecked $ do
        dlChecked <-
          maybe
            (pure False)
            (\_ -> checkDL merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.PanCard)
            dlNumber
        unless dlChecked $
          when (isJust gstNumber) $
            checkGST personId mbPanNumber
    ODC.DriverLicense -> do
      aadhaarChecked <-
        maybe
          (pure False)
          (\_ -> checkAadhaar merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.DriverLicense)
          aadhaarNumber
      unless aadhaarChecked $
        whenJust panNumber $ \_ ->
          void $ checkPan merchantId merchantOpCityId personId mbNameValue mbUtcDateOfBirthValue ODC.DriverLicense
    ODC.GSTCertificate -> checkTwoPanNumber mbPanNumber panNumber
    _ -> return ()

checkTwoPanNumber :: Maybe Text -> Maybe Text -> Flow ()
checkTwoPanNumber mbExtractedValue mbVerifiedValue = do
  case (mbExtractedValue, mbVerifiedValue) of
    (Just extractedValue, Just verifiedValue) -> do
      unless (extractedValue == verifiedValue) $ throwError (MismatchDataError "GST not linked with existing PAN")
      return ()
    _ -> return ()

validateNameAndDOB :: Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Maybe Text -> Maybe Text -> Maybe UTCTime -> Maybe UTCTime -> Id Person.Person -> Flow Bool
validateNameAndDOB merchantId merchantOpCityId mbExtractedName mbVerifiedName mbExtractedDOB mbVerifiedDOB personId = do
  isNameValid <- compareNames merchantId merchantOpCityId mbExtractedName mbVerifiedName personId
  isDateOfBirthValid <- compareDateOfBirth mbExtractedDOB mbVerifiedDOB
  return (isNameValid && isDateOfBirthValid)

-- (P = Individual, C = Company, H = HUF, F = Firm, A = AOP, T = Trust, B = BOI, L/J/G = others).
inferPanTypeFromNumber :: Text -> Maybe DPan.PanType
inferPanTypeFromNumber panNumber =
  let upperPan = T.toUpper panNumber
   in if T.length upperPan >= 4
        then Just $ if T.index upperPan 3 == 'P' then DPan.INDIVIDUAL else DPan.BUSINESS
        else Nothing

isBusinessPan :: Maybe Text -> Bool
isBusinessPan = maybe False ((== Just DPan.BUSINESS) . inferPanTypeFromNumber)

-- | When a merchant enables individualPANCheck, reject business PANs (4th char ≠ 'P')
-- for individual drivers (incl. fleet drivers) and individual fleet owners.
-- Business fleet owners and other roles are exempt, as they may hold a business PAN.
validateIndividualPANCheck :: DTC.TransporterConfig -> Person.Person -> Text -> Flow ()
validateIndividualPANCheck transporterConfig person panNumber =
  when (transporterConfig.individualPANCheck == Just True && isIndividualRole person.role) $
    when (inferPanTypeFromNumber (removeSpaceAndDash panNumber) /= Just DPan.INDIVIDUAL) $
      throwError (InvalidRequest "Business PAN card not be accepted please upload individual PAN")
  where
    isIndividualRole role = role == Person.DRIVER || role == Person.FLEET_OWNER

-- Define a common function to handle role-based decryption
getDriverDocumentInfo :: Person.Person -> Flow (Bool, DriverDocument)
getDriverDocumentInfo person = do
  case person.role of
    role | role `elem` [Person.FLEET_OWNER, Person.FLEET_BUSINESS] -> do
      res <- FOI.findByPrimaryKey person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedGstNumber <- mapM decrypt res.gstNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber Nothing decryptedGstNumber)
    _ -> do
      res <- DIQuery.findById person.id >>= fromMaybeM (PersonNotFound person.id.getId)
      decryptedPanNumber <- mapM decrypt res.panNumber
      decryptedAadhaarNumber <- mapM decrypt res.aadhaarNumber
      decryptedDlNumber <- mapM decrypt res.dlNumber
      return (res.blocked, DriverDocument decryptedPanNumber decryptedAadhaarNumber decryptedDlNumber Nothing)

-- | Returns True if name compare is required for the given transporterConfig and verifyBy
isNameCompareRequired :: DTC.TransporterConfig -> DPan.VerifiedBy -> Bool
isNameCompareRequired transporterConfig verifyBy =
  isJust transporterConfig.validNameComparePercentage && verifyBy /= DPan.DASHBOARD_ADMIN && verifyBy /= DPan.DASHBOARD_USER

makeDocumentVerificationLockKey :: Text -> Text
makeDocumentVerificationLockKey personId = "DocumentVerificationLock:" <> personId

endFleetRCAssociationIfPossible ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  Id Person ->
  Id VehicleRegistrationCertificate ->
  m ()
endFleetRCAssociationIfPossible fleetOwnerId rcId = do
  now <- getCurrentTime
  mbFleetRc <- FRCAssoc.findLinkedByRCIdAndFleetOwnerId fleetOwnerId rcId now
  whenJust mbFleetRc $ \fleetRc -> FRCAssoc.endById fleetRc.id
