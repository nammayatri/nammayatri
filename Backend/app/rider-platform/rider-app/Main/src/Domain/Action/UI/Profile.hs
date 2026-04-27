{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    UpdateProfileResp,
    MarketEventReq (..),
    getPersonDetails,
    updatePerson,
    triggerUpdateAuthDataOtp,
    verifyUpdateAuthDataOtp,
    marketingEvents,
    VerifyUpdateAuthOTPReq (..),
    TriggerUpdateAuthOTPReq (..),
    AuthData (..),
  )
where

import qualified BecknV2.OnDemand.Enums as BecknEnums
import qualified BecknV2.OnDemand.Enums as Enums
import Control.Applicative ((<|>))
import Data.Aeson as DA
import qualified Data.Aeson.KeyMap as DAKM
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Digest.Pure.MD5 as MD5
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Action.UI.Registration as DR
import Domain.Types.Booking as DBooking
import qualified Domain.Types.ClientPersonInfo as DCP
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.PersonDisability as PersonDisability
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.VehicleCategory as VehicleCategory
import qualified Email.Types
import Environment
import qualified EulerHS.Language as L
import EulerHS.Prelude (concatMapM)
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.External.Types (SchedulerFlow, SchedulerType, ServiceFlow)
import qualified Kernel.External.Whatsapp.Interface.Types as Whatsapp (OptApiMethods)
import Kernel.Prelude
import Kernel.Sms.Config (SmsConfig, useFakeSms)
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Streaming.Kafka.Producer.Types (KafkaProducerTools)
import Kernel.Tools.Metrics.CoreMetrics
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Kernel.Utils.Version
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Tools.Utils as YUtils
import qualified Lib.Yudhishthira.Types as LYT
import qualified Safety.API.Types.UI.PersonDefaultEmergencyNumber as EmergencyAPI
import qualified Safety.API.Types.UI.SafetySettings as SafetyAPI
import qualified Safety.Domain.Action.UI.PersonDefaultEmergencyNumber as EmergencyLib
import qualified Safety.Domain.Action.UI.SafetySettings as SafetySettingsLib
import qualified Safety.Domain.Types.Common as SafetyCommon
import qualified Safety.Domain.Types.PersonDefaultEmergencyNumber as SafetyPDEN
import qualified Safety.Storage.Queries.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Safety.Storage.Queries.SafetySettingsExtra as Lib
import qualified SharedLogic.BehaviourManagement.CustomerCancellationRate as CCR
import SharedLogic.Cac
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.OTP as SOTP
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import qualified SharedLogic.Referral as Referral
import Storage.Beam.Sos ()
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import Storage.ConfigPilot.Config.PayoutConfig (PayoutDimensions (..))
import Storage.ConfigPilot.Config.RiderConfig (RiderDimensions (..))
import Storage.ConfigPilot.Interface.Types (getConfig, getOneConfig)
import Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ClientPersonInfo as QCP
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.PersonStats as QPersonStats
import Text.Regex.Posix ((=~))
import Tools.Error
import Tools.Event

-- Email validation function
isValidEmail :: Maybe Text -> Bool
isValidEmail Nothing = False
isValidEmail (Just email) =
  let trimmedEmail = T.strip email
      emailRegex = "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$" :: String
   in T.unpack trimmedEmail =~ emailRegex

data ProfileRes = ProfileRes
  { id :: Id Person.Person,
    firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    maskedMobileNumber :: Maybe Text,
    maskedDeviceToken :: Maybe Text,
    isBlocked :: Bool,
    hasTakenRide :: Bool,
    hasTakenValidRide :: Bool,
    hasTakenValidAutoRide :: Bool,
    hasTakenValidCabRide :: Bool,
    hasTakenValidBikeRide :: Bool,
    hasTakenValidAmbulanceRide :: Bool,
    hasTakenValidTruckRide :: Bool,
    businessEmail :: Maybe Text,
    businessProfileVerified :: Maybe Bool,
    referralCode :: Maybe Text,
    whatsappNotificationEnrollStatus :: Maybe Whatsapp.OptApiMethods,
    language :: Maybe Maps.Language,
    hasDisability :: Maybe Bool,
    disability :: Maybe Text,
    gender :: Person.Gender,
    hasCompletedSafetySetup :: Bool,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    followsRide :: Bool,
    frontendConfigHash :: Maybe Text,
    isSafetyCenterDisabled :: Bool,
    customerReferralCode :: Maybe Text,
    deviceId :: Maybe Text,
    androidId :: Maybe Text,
    aadhaarVerified :: Bool,
    hasTakenValidBusRide :: Bool,
    payoutVpa :: Maybe Text,
    referralEarnings :: Maybe HighPrecMoney,
    referredByEarnings :: Maybe HighPrecMoney,
    referralAmountPaid :: Maybe HighPrecMoney,
    cancellationRate :: Maybe Int,
    isPayoutEnabled :: Maybe Bool,
    publicTransportVersion :: Maybe Text,
    isMultimodalRider :: Bool,
    customerTags :: DA.Value,
    profilePicture :: Maybe Text,
    paymentMode :: Maybe DMPM.PaymentMode,
    blockedUntil :: Maybe UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

data UpdateProfileReq = UpdateProfileReq
  { firstName :: Maybe Text,
    middleName :: Maybe Text,
    lastName :: Maybe Text,
    email :: Maybe Text,
    deviceToken :: Maybe Text,
    notificationToken :: Maybe Text,
    referralCode :: Maybe Text,
    language :: Maybe Maps.Language,
    gender :: Maybe Person.Gender,
    bundleVersion :: Maybe Version,
    clientVersion :: Maybe Version,
    businessEmail :: Maybe Text,
    disability :: Maybe Disability,
    hasDisability :: Maybe Bool,
    enableOtpLessRide :: Maybe Bool,
    deviceId :: Maybe Text,
    androidId :: Maybe Text,
    liveActivityToken :: Maybe Text,
    dateOfBirth :: Maybe UTCTime,
    profilePicture :: Maybe Text,
    verificationChannel :: Maybe Text,
    registrationLat :: Maybe Double,
    registrationLon :: Maybe Double,
    latestLat :: Maybe Double,
    latestLon :: Maybe Double,
    marketingParams :: Maybe MarketingParams,
    mbMobileNumber :: Maybe Text,
    mbMobileCountryCode :: Maybe Text,
    paymentMode :: Maybe DMPM.PaymentMode,
    driverPreference :: Maybe [Text]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MarketEventReq = MarketEventReq
  { marketingParams :: MarketingParams
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data MarketingParams = MarketingParams
  { gclId :: Maybe Text,
    utmCampaign :: Maybe Text,
    utmContent :: Maybe Text,
    utmCreativeFormat :: Maybe Text,
    utmMedium :: Maybe Text,
    utmSource :: Maybe Text,
    utmTerm :: Maybe Text,
    userType :: Maybe UserType,
    appName :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateProfileResp = APISuccess.APISuccess

data Disability = Disability
  { id :: Id Disability,
    description :: Maybe Text
  }
  deriving (Show, Eq, Generic, ToSchema, ToJSON, FromJSON)

getIsMultimodalRider :: Maybe Bool -> Maybe [LYT.TagNameValueExpiry] -> [a] -> Bool
getIsMultimodalRider enableMultiModalForAllUsers mbTags integratedBPPConfigs =
  case enableMultiModalForAllUsers of
    Just True -> not (null integratedBPPConfigs)
    _ ->
      let multimodalTagName = LYT.TagName "MultimodalRider"
          currentTags = fromMaybe [] mbTags
          isMultimodalRiderTag targetTagName customerTag =
            case YUtils.parseTagName customerTag of
              Just tagName -> tagName == targetTagName
              Nothing -> False
       in any (isMultimodalRiderTag multimodalTagName) currentTags && not (null integratedBPPConfigs)

getPersonDetails ::
  ( HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType],
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r,
    EncFlow m r,
    HasShortDurationRetryCfg r c,
    SchedulerFlow r,
    ServiceFlow m r,
    HasField "maxShards" r Int,
    HasField "schedulerSetName" r Text,
    HasField "schedulerType" r SchedulerType,
    HasField "jobInfoMap" r (M.Map Text Bool),
    HasField "blackListedJobs" r [Text]
  ) =>
  (Id Person.Person, Id Merchant.Merchant) ->
  Maybe Int ->
  Maybe Text ->
  Maybe Text ->
  Maybe Bool ->
  Maybe Version ->
  Maybe Text ->
  Maybe Version ->
  Maybe Version ->
  Maybe Text ->
  Maybe Text ->
  m ProfileRes
getPersonDetails (personId, _) toss tenant' context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = do
  logInfo $ "[Profile.getPersonDetails] START | personId: " <> personId.getId
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (isNothing person.clientId && isJust mbClientId) $ QPerson.updateClientId mbClientId personId
  logInfo "[Profile.getPersonDetails] findById done"
  decPerson <- decrypt person
  logInfo "[Profile.getPersonDetails] decrypt done"
  personStats <- QPersonStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let device = getDeviceFromText mbDevice
      totalRides = personStats.completedRides + personStats.driverCancelledRides + personStats.userCancelledRides
      rate = (personStats.userCancelledRides * 100) `div` max 1 totalRides
      sendCancellationRate = totalRides >= (fromMaybe 1000 riderConfig.minRidesToShowCancellationRate) && personStats.isBackfilled == Just True
  let cancellationPerc = if sendCancellationRate then Just rate else Nothing
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PDisability.findByPersonId personId
    _ -> return Nothing
  logInfo "[Profile.getPersonDetails] disability check done"

  cloudType <- asks (.cloudType)
  when ((decPerson.clientBundleVersion /= mbBundleVersion || decPerson.clientSdkVersion /= mbClientVersion || decPerson.clientConfigVersion /= mbClientConfigVersion || decPerson.clientReactNativeVersion /= mbRnVersion || decPerson.clientDevice /= device || person.cloudType /= cloudType) && isJust device) do
    deploymentVersion <- asks (.version)
    void $ QPerson.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion device deploymentVersion.getDeploymentVersion mbRnVersion cloudType
  logInfo "[Profile.getPersonDetails] version update check done"
  when (isJust decPerson.email && not (isValidEmail decPerson.email)) do
    logDebug $ "Invalid email, updating person email to nothing , Previous emailId: " <> show decPerson.email <> " for person id " <> show personId
    let updatedPerson = person {Person.email = Nothing}
    void $ QPerson.updateByPrimaryKey updatedPerson
  logInfo "[Profile.getPersonDetails] email validation done"
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  let context' = fromMaybe DAKM.empty (DA.decode $ BSL.pack $ T.unpack $ fromMaybe "{}" context)
  logInfo $ "[Profile.getPersonDetails] calling getFrontendConfigs, useCACConfig=" <> show useCACConfig
  frntndfgs <- if useCACConfig then getFrontendConfigs person toss tenant' context' else return $ Just DAKM.empty
  logInfo "[Profile.getPersonDetails] getFrontendConfigs done"
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  safetySettings <- Lib.findSafetySettingsWithFallback (cast personId) (Lib.getDefaultSafetySettings (cast personId) (Just $ SLP.riderPersonToSafetySettingsPersonDefaults person))
  logInfo "[Profile.getPersonDetails] findSafetySettings done"
  isSafetyCenterDisabled_ <- SLP.checkSafetyCenterDisabled person safetySettings
  hasTakenValidRide <- QCP.findAllByPersonId personId
  logInfo "[Profile.getPersonDetails] findAllByPersonId (ClientPersonInfo) done"
  let hasTakenValidFirstCabRide = validRideCount hasTakenValidRide BecknEnums.CAB
      hasTakenValidFirstAutoRide = validRideCount hasTakenValidRide BecknEnums.AUTO_RICKSHAW
      hasTakenValidFirstBikeRide = validRideCount hasTakenValidRide BecknEnums.MOTORCYCLE
      hasTakenValidAmbulanceRide = validRideCount hasTakenValidRide BecknEnums.AMBULANCE
      hasTakenValidTruckRide = validRideCount hasTakenValidRide BecknEnums.TRUCK
      hasTakenValidBusRide = validRideCount hasTakenValidRide BecknEnums.BUS
  newCustomerReferralCode <-
    if isNothing person.customerReferralCode
      then do
        logInfo "[Profile.getPersonDetails] generating new customerReferralCode"
        newCustomerReferralCode <- DR.generateCustomerReferralCode
        checkIfReferralCodeExists <- QPerson.findPersonByCustomerReferralCode (Just newCustomerReferralCode)
        if isNothing checkIfReferralCodeExists
          then do
            void $ QPerson.updateCustomerReferralCode personId newCustomerReferralCode
            pure $ Just newCustomerReferralCode
          else pure Nothing
      else pure person.customerReferralCode
  logInfo "[Profile.getPersonDetails] referralCode done"
  mbPayoutConfig <- getOneConfig (PayoutDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId, vehicleCategory = Just VehicleCategory.AUTO_CATEGORY, isPayoutEnabled = Nothing, payoutEntity = Nothing})
  logInfo "[Profile.getPersonDetails] findPayoutConfig done"
  let vehicleTypes = [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes
  logInfo $ "[Profile.getPersonDetails] findIntegratedBPPConfigs done, count=" <> show (length integratedBPPConfigs)
  let isMultimodalRider = getIsMultimodalRider riderConfig.enableMultiModalForAllUsers decPerson.customerNammaTags integratedBPPConfigs
  -- Check if customer is temporarily blocked and should be unblocked
  fork "Check and unblock customer if temporary block expired" $ do
    when person.blocked $ do
      case person.blockedUntil of
        Just blockedUntilTime -> do
          now <- getCurrentTime
          when (now > blockedUntilTime) $ do
            void $ QPerson.updatingEnabledAndBlockedState personId Nothing False
            logInfo $ "Unblocked customer in profile API, customerId: " <> personId.getId <> " blockedUntil was: " <> show blockedUntilTime
        Nothing -> pure ()
  fork "Check customer cancellation rate blocking" $ CCR.nudgeOrBlockCustomer riderConfig person
  logInfo "[Profile.getPersonDetails] calling makeProfileRes (includes getGtfsVersion)"
  makeProfileRes riderConfig decPerson tag mbMd5Digest isSafetyCenterDisabled_ newCustomerReferralCode hasTakenValidFirstCabRide hasTakenValidFirstAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide hasTakenValidTruckRide hasTakenValidBusRide safetySettings personStats cancellationPerc mbPayoutConfig integratedBPPConfigs isMultimodalRider includeProfileImage
  where
    makeProfileRes riderConfig Person.Person {..} disability md5DigestHash isSafetyCenterDisabled_ newCustomerReferralCode hasTakenCabRide hasTakenAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide hasTakenValidTruckRide hasTakenValidBusRide safetySettings personStats cancellationPerc mbPayoutConfig integratedBPPConfigs isMultimodalRider includeProfileImageParam = do
      logInfo $ "[Profile.makeProfileRes] calling getGtfsVersion for " <> show (length integratedBPPConfigs) <> " configs"
      gtfsVersion <-
        withTryCatch "getGtfsVersion:getPersonDetails" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
          Left _ -> return (map (.feedKey) integratedBPPConfigs)
          Right gtfsVersions -> return gtfsVersions
      logInfo "[Profile.makeProfileRes] getGtfsVersion done"
      return $
        ProfileRes
          { maskedMobileNumber = maskText <$> mobileNumber,
            maskedDeviceToken = maskText <$> deviceToken,
            hasTakenRide = hasTakenValidRide,
            frontendConfigHash = md5DigestHash,
            hasTakenValidAutoRide = hasTakenAutoRide,
            hasTakenValidCabRide = hasTakenCabRide,
            hasTakenValidBikeRide = hasTakenValidFirstBikeRide,
            hasTakenValidAmbulanceRide = hasTakenValidAmbulanceRide,
            hasTakenValidTruckRide = hasTakenValidTruckRide,
            hasTakenValidBusRide = hasTakenValidBusRide,
            isSafetyCenterDisabled = isSafetyCenterDisabled_,
            customerReferralCode = newCustomerReferralCode,
            bundleVersion = clientBundleVersion,
            clientVersion = clientSdkVersion,
            deviceId = maskText <$> deviceId,
            androidId = maskText <$> androidId,
            hasCompletedMockSafetyDrill = safetySettings.hasCompletedMockSafetyDrill,
            hasCompletedSafetySetup = safetySettings.hasCompletedSafetySetup,
            isBlocked = blocked,
            referralEarnings = Just personStats.referralEarnings,
            referredByEarnings = Just personStats.referredByEarnings,
            referralAmountPaid = Just personStats.referralAmountPaid,
            isPayoutEnabled = mbPayoutConfig <&> (.isPayoutEnabled),
            cancellationRate = cancellationPerc,
            publicTransportVersion = if null gtfsVersion then Nothing else Just (T.intercalate (T.pack "#") gtfsVersion <> (maybe "" (\version -> "#" <> show version) riderConfig.domainPublicTransportDataVersion)),
            customerTags = YUtils.convertTags $ fromMaybe [] customerNammaTags,
            profilePicture = if includeProfileImageParam == Just True then profilePicture else Nothing,
            ..
          }

validRideCount :: [DCP.ClientPersonInfo] -> BecknEnums.VehicleCategory -> Bool
validRideCount hasTakenValidRide vehicleCategory =
  case find (\info -> info.vehicleCategory == Just vehicleCategory) hasTakenValidRide of
    Just info -> info.rideCount == 1
    Nothing -> False

marketingEvents :: (HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "version" ::: DeploymentVersion], CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r, HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools], EventStreamFlow m r) => MarketEventReq -> m APISuccess.APISuccess
marketingEvents req = do
  let params = req.marketingParams
  now <- getCurrentTime
  let marketingParams = MarketingParamsEventPreLoginData params.gclId params.utmCampaign params.utmContent params.utmCreativeFormat params.utmMedium params.utmSource params.utmTerm params.appName params.userType now now
  triggerMarketingParamEventPreLogin marketingParams
  pure APISuccess.Success

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, EventStreamFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "version" ::: DeploymentVersion, "cloudType" ::: Maybe CloudType], HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]) => Id Person.Person -> Id Merchant.Merchant -> UpdateProfileReq -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> Maybe Text -> m APISuccess.APISuccess
updatePerson personId merchantId req mbRnVersion mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice mbClientId = do
  mPerson <- join <$> QPerson.findByEmailAndMerchantId merchantId `mapM` req.email
  whenJust mPerson (\person -> when (person.id /= personId) $ throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email
  mbEncBusinessEmail <- encrypt `mapM` req.businessEmail
  deploymentVersion <- asks (.version)
  cloudType <- asks (.cloudType)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  when (isNothing person.clientId && isJust mbClientId) $ QPerson.updateClientId mbClientId personId
  fork "Triggering kafka marketing params event for person" $
    case req.marketingParams of
      Just params -> do
        now <- getCurrentTime
        let marketingParams = MarketingParamsEventData person.id params.gclId params.utmCampaign params.utmContent params.utmCreativeFormat params.utmMedium params.utmSource params.utmTerm params.appName merchantId person.merchantOperatingCityId params.userType now now
        triggerMarketingParamEvent marketingParams
      Nothing -> pure ()
  -- TODO: Remove this part from here once UI stops using updatePerson api to apply referral code
  void $ mapM (\refCode -> Referral.applyReferralCode person False refCode Nothing) req.referralCode
  void $
    QPerson.updatePersonalInfo
      personId
      req.firstName
      req.middleName
      req.lastName
      mbEncEmail
      mbEncBusinessEmail
      req.deviceToken
      req.notificationToken
      req.language
      req.gender
      mbRnVersion
      (mbClientVersion <|> req.clientVersion)
      (mbBundleVersion <|> req.bundleVersion)
      mbClientConfigVersion
      (getDeviceFromText mbDevice)
      deploymentVersion.getDeploymentVersion
      req.enableOtpLessRide
      (if isJust person.deviceId then Nothing else req.deviceId)
      (if isJust person.androidId then Nothing else req.androidId)
      req.dateOfBirth
      req.profilePicture
      req.verificationChannel
      req.registrationLat
      req.registrationLon
      req.latestLat
      req.latestLon
      person
      req.liveActivityToken
      Nothing
      Nothing
      req.paymentMode
      cloudType
  _ <- updateDisability req.hasDisability req.disability personId
  whenJust req.driverPreference $ \prefs -> do
    let driverPreferenceTagName = LYT.TagNameValueExpiry "driverPreference#"
        normalizedPrefs =
          prefs
            <&> T.strip
            & filter (\p -> not (T.null p) && not ("#" `T.isInfixOf` p) && not ("&" `T.isInfixOf` p))
        newTags =
          if null normalizedPrefs
            then Just $ YUtils.removeTagName person.customerNammaTags driverPreferenceTagName
            else Just $ YUtils.replaceTagNameValue person.customerNammaTags (LYT.TagNameValueExpiry $ "driverPreference#" <> T.intercalate "&" normalizedPrefs)
    unless ((YUtils.showRawTags <$> newTags) == (YUtils.showRawTags <$> person.customerNammaTags)) $
      QPerson.updateCustomerTags newTags personId
  pure APISuccess.Success

updateDisability :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Maybe Bool -> Maybe Disability -> Id Person.Person -> m APISuccess.APISuccess
updateDisability hasDisability mbDisability personId = do
  case (hasDisability, mbDisability) of
    (Nothing, _) -> logDebug "No Disability"
    (Just False, _) -> do
      QPerson.updateHasDisability (Just False) personId
      PDisability.deleteByPersonId personId
    (Just True, Nothing) -> throwError $ InvalidRequest "Field disability can't be null if hasDisability is True"
    (Just True, Just selectedDisability) -> do
      customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
      QPerson.updateHasDisability (Just True) personId
      let disabilityId = getId $ selectedDisability.id
      disability <- runInReplica $ QD.findByDisabilityId disabilityId >>= fromMaybeM (DisabilityDoesNotExist disabilityId)
      let mbDescription = (selectedDisability.description) <|> (Just disability.description)
      when (isNothing customerDisability) $ do
        newDisability <- makeDisability selectedDisability disability.tag mbDescription
        PDisability.create newDisability
      when (isJust customerDisability) $ do
        PDisability.updateDisabilityByPersonId disabilityId disability.tag mbDescription personId
      where
        makeDisability personDisability tag mbDescription = do
          now <- getCurrentTime
          return $
            PersonDisability.PersonDisability
              { personId = personId,
                disabilityId = getId $ personDisability.id,
                tag = tag,
                description = mbDescription,
                createdAt = now,
                updatedAt = now
              }
  pure APISuccess.Success

instance EmergencyLib.HasEmergencyContactHandle AppEnv Flow where
  getEmergencyContactHandle =
    pure
      EmergencyLib.ServiceHandle
        { EmergencyLib.buildContacts = riderBuildContacts,
          EmergencyLib.enrichContact = riderEnrichContact,
          EmergencyLib.validatePutRequest = riderValidatePutRequest,
          EmergencyLib.getPersonDefaults = riderGetPersonDefaults,
          EmergencyLib.handleAfterPut = riderHandleAfterPut
        }

riderValidatePutRequest ::
  Id SafetyCommon.Person ->
  EmergencyAPI.UpdateEmergencyContactsReq ->
  Flow ()
riderValidatePutRequest _ req = do
  maxCount <- asks (.maxEmergencyNumberCount)
  runRequestValidation (validateUpdateEmergencyContactsReq maxCount) req
  where
    validateUpdateEmergencyContactsReq maxCount EmergencyAPI.UpdateEmergencyContactsReq {..} =
      sequenceA_
        [ validateList "defaultEmergencyNumbers" defaultEmergencyNumbers validateEmergencyContactReq,
          validateField "defaultEmergencyNumbers" defaultEmergencyNumbers $ MaxLength maxCount
        ]
    validateEmergencyContactReq EmergencyAPI.EmergencyContactReq {..} =
      sequenceA_
        [ validateField "mobileNumber" mobileNumber P.mobileNumber,
          validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
        ]

riderGetPersonDefaults ::
  Id SafetyCommon.Person ->
  Flow (Maybe Lib.SafetySettingsPersonDefaults)
riderGetPersonDefaults pid = do
  person <- runInReplica $ QPerson.findById (cast pid) >>= fromMaybeM (PersonNotFound (pid.getId))
  pure $ Just (SLP.riderPersonToSafetySettingsPersonDefaults person)

riderBuildContacts ::
  Id SafetyCommon.Person ->
  Id SafetyCommon.Merchant ->
  [EmergencyAPI.EmergencyContactReq] ->
  Flow [SafetyPDEN.PersonDefaultEmergencyNumber]
riderBuildContacts personId merchantId contacts = do
  person <- runInReplica $ QPerson.findById (cast personId) >>= fromMaybeM (PersonNotFound (personId.getId))
  now <- getCurrentTime
  -- If any contact lacks shareTripWithEmergencyContactOption, fall back to the person-level setting.
  let shareTripOptionDefault =
        if any (isNothing . (.shareTripWithEmergencyContactOption)) contacts
          then DPDEN.toSafetyRideShare <$> person.shareTripWithEmergencyContactOption
          else Nothing
  mapM (buildOne now shareTripOptionDefault) contacts
  where
    buildOne now shareTripOptionDefault contact = do
      encMobNum <- encrypt contact.mobileNumber
      dbHash <- getDbHash contact.mobileNumber
      mbEmNumPerson <- QPerson.findByMobileNumberAndMerchantId contact.mobileCountryCode dbHash (cast merchantId)
      let resolvedShareOpt =
            contact.shareTripWithEmergencyContactOption
              <|> if contact.enableForFollowing == Just True
                then shareTripOptionDefault
                else Just SafetyCommon.NEVER_SHARE
      pure
        SafetyPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = contact.name,
            mobileCountryCode = contact.mobileCountryCode,
            createdAt = now,
            contactPersonId = cast . (.id) <$> mbEmNumPerson,
            enableForFollowing = fromMaybe False contact.enableForFollowing,
            enableForShareRide = False,
            priority = fromMaybe 1 contact.priority,
            merchantId = Just merchantId,
            personId = personId,
            shareTripWithEmergencyContactOption = resolvedShareOpt
          }

riderEnrichContact ::
  SafetyPDEN.DecryptedPersonDefaultEmergencyNumber ->
  Flow EmergencyAPI.EmergencyContact
riderEnrichContact personEN = do
  onRide <- case (personEN.contactPersonId, personEN.priority == 0) of
    (Just contactId, True) -> do
      activeBookings <- runInReplica $ QBooking.findByRiderIdAndStatus (cast contactId) DBooking.activeBookingStatus
      return $ not $ null activeBookings
    _ -> return False
  pure
    EmergencyAPI.EmergencyContact
      { name = personEN.name,
        mobileCountryCode = personEN.mobileCountryCode,
        mobileNumber = personEN.mobileNumber,
        priority = personEN.priority,
        contactPersonId = cast <$> personEN.contactPersonId,
        merchantId = cast <$> personEN.merchantId,
        enableForFollowing = personEN.enableForFollowing,
        enableForShareRide = personEN.enableForShareRide,
        shareTripWithEmergencyContactOption = personEN.shareTripWithEmergencyContactOption,
        onRide = Just onRide
      }

riderHandleAfterPut ::
  Id SafetyCommon.Person ->
  [SafetyPDEN.PersonDefaultEmergencyNumber] ->
  [SafetyPDEN.PersonDefaultEmergencyNumber] ->
  Flow ()
riderHandleAfterPut personId newList oldList =
  fork "Send Emergency Contact Added Message" $ do
    decNew <- mapM decrypt newList
    decOld <- mapM decrypt oldList
    let oldMobileNumbers = (.mobileNumber) <$> decOld
        newAPIList = flip map decNew $ \personEN ->
          EmergencyAPI.EmergencyContact
            { name = personEN.name,
              mobileCountryCode = personEN.mobileCountryCode,
              mobileNumber = personEN.mobileNumber,
              priority = personEN.priority,
              contactPersonId = cast <$> personEN.contactPersonId,
              merchantId = cast <$> personEN.merchantId,
              enableForFollowing = personEN.enableForFollowing,
              enableForShareRide = personEN.enableForShareRide,
              shareTripWithEmergencyContactOption = personEN.shareTripWithEmergencyContactOption,
              onRide = Nothing
            }
    person <- runInReplica $ QPerson.findById (cast personId) >>= fromMaybeM (PersonNotFound (personId.getId))
    riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
    buildSmsReq <-
      MessageBuilder.buildAddedAsEmergencyContactMessage person.merchantOperatingCityId $
        MessageBuilder.BuildAddedAsEmergencyContactMessageReq
          { userName = SLP.getName person,
            appUrl = riderConfig.appUrl
          }
    let filterNewContacts personDEN = return $ not $ personDEN.mobileNumber `elem` oldMobileNumbers
    newlyAddedContacts <- filterM filterNewContacts newAPIList
    SPDEN.notifyEmergencyContacts person (notificationMessage person) "Emergency Contact Added" Notification.EMERGENCY_CONTACT_ADDED (Just buildSmsReq) riderConfig.enableEmergencyContactAddedMessage newlyAddedContacts Nothing
  where
    notificationMessage person = SLP.getName person <> " has added you as the emergency contact."

instance SafetySettingsLib.HasSafetySettingsHandle AppEnv Flow where
  getSafetySettingsHandle =
    pure
      SafetySettingsLib.ServiceHandle
        { SafetySettingsLib.getPlatformExtras = riderGetPlatformExtras,
          SafetySettingsLib.handlePlatformUpdate = riderHandlePlatformUpdate
        }

riderGetPlatformExtras ::
  Id SafetyCommon.Person ->
  Flow (Maybe Lib.SafetySettingsPersonDefaults, Maybe SafetySettingsLib.SafetyRiderExtras)
riderGetPlatformExtras pid = do
  person <- runInReplica $ QPerson.findById (cast pid) >>= fromMaybeM (PersonNotFound (pid.getId))
  personENList <- DPDEN.findpersonENListWithFallBack (cast pid) (Just person)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  -- Find the first non-NEVER_SHARE option across all contacts.
  -- Both shareTripWithEmergencyContacts and shareTripWithEmergencyContactOption
  -- derive from this so they are always consistent with each other.
  let aggregatedShareOption =
        listToMaybe
          [ opt
            | x <- personENList,
              Just opt <- [SafetyPDEN.shareTripWithEmergencyContactOption x],
              opt /= SafetyCommon.NEVER_SHARE
          ]
      personDefaults = Just (SLP.riderPersonToSafetySettingsPersonDefaults person)
      riderExtras =
        Just
          SafetySettingsLib.SafetyRiderExtras
            { shareTripWithEmergencyContacts = isJust aggregatedShareOption,
              shareTripWithEmergencyContactOption = fromMaybe SafetyCommon.NEVER_SHARE aggregatedShareOption,
              enablePoliceSupport = riderConfig.enableLocalPoliceSupport,
              localPoliceNumber = riderConfig.localPoliceNumber,
              safetyCheckStartTime = riderConfig.safetyCheckStartTime,
              safetyCheckEndTime = riderConfig.safetyCheckEndTime
            }
  pure (personDefaults, riderExtras)

riderHandlePlatformUpdate ::
  Id SafetyCommon.Person ->
  SafetyAPI.UpdateSafetySettingsReq ->
  Flow ()
riderHandlePlatformUpdate personId req = do
  personENList <- QPersonDEN.findAllByPersonId personId
  let wouldEnableContactSharing =
        fromMaybe False req.shareEmergencyContacts
          || req.autoCallDefaultContact == Just True
          || req.notifySosWithEmergencyContacts == Just True
  when (wouldEnableContactSharing && null personENList) do
    throwError (InvalidRequest "Add atleast one emergency contact.")
  -- shareTripWithEmergencyContactOption takes precedence; shareTripWithEmergencyContacts
  -- is a convenience bool that maps True → SHARE_WITH_TIME_CONSTRAINTS, False → NEVER_SHARE.
  let shareTripOptions =
        req.shareTripWithEmergencyContactOption
          <|> fmap (bool SafetyCommon.NEVER_SHARE SafetyCommon.SHARE_WITH_TIME_CONSTRAINTS) req.shareTripWithEmergencyContacts
  when (isJust shareTripOptions) $
    QPersonDEN.updateShareTripWithEmergencyContactOptions personId shareTripOptions

data TriggerUpdateAuthOTPReq = TriggerUpdateAuthOTPReq
  { identifier :: Maybe SP.IdentifierType,
    email :: Maybe Text,
    mobileNumber :: Maybe Text,
    mobileCountryCode :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

data AuthData = AuthData
  { mobileNumber :: Maybe Text,
    mobileNumberCountryCode :: Maybe Text,
    email :: Maybe Text,
    otp :: DbHash
  }
  deriving (Generic, Show, FromJSON, ToJSON)

triggerUpdateAuthDataOtp ::
  ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
    HasField "emailServiceConfig" r Email.Types.EmailServiceConfig,
    CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  (Id Person.Person, Id Merchant.Merchant) ->
  TriggerUpdateAuthOTPReq ->
  m APISuccess.APISuccess
triggerUpdateAuthDataOtp (personId, _merchantId) req = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  smsCfg <- asks (.smsCfg)
  riderConfig <- getConfig (RiderDimensions {merchantOperatingCityId = person.merchantOperatingCityId.getId}) >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  identifierType <- req.identifier & fromMaybeM (InvalidRequest "Identifier type is required")
  let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp

  generatedOtpCode <- maybe generateOTPCode return useFakeOtpM
  let otpChannel = case identifierType of
        SP.MOBILENUMBER -> SOTP.SMS
        SP.EMAIL -> SOTP.EMAIL
        SP.AADHAAR -> SOTP.SMS
        SP.CONDUCTORTOKEN -> SOTP.SMS

  case identifierType of
    SP.MOBILENUMBER -> do
      countryCode <- req.mobileCountryCode & fromMaybeM (InvalidRequest "MobileCountryCode is required for MOBILENUMBER identifier")
      mobileNumber <- req.mobileNumber & fromMaybeM (InvalidRequest "MobileNumber is required for MOBILENUMBER identifier")

      mobileNumberDbHash <- getDbHash mobileNumber
      mobileNumberExists <- QPerson.findByMobileNumberAndMerchantId countryCode mobileNumberDbHash person.merchantId
      whenJust mobileNumberExists $ \existing ->
        when (existing.id /= personId) $ throwError (InvalidRequest "Phone number already registered")

      storeAndSendOTP generatedOtpCode identifierType personId person smsCfg useFakeOtpM otpChannel riderConfig req (Just mobileNumber) (Just countryCode) Nothing
    SP.EMAIL -> do
      receiverEmail <- req.email & fromMaybeM (InvalidRequest "Email is required for EMAIL identifier")
      existingPerson <- QPerson.findByEmailAndMerchantId person.merchantId receiverEmail
      whenJust existingPerson $ \existing ->
        when (existing.id /= personId) $ throwError $ InvalidRequest "Email already registered"
      storeAndSendOTP generatedOtpCode identifierType personId person smsCfg useFakeOtpM otpChannel riderConfig req Nothing Nothing (Just receiverEmail)
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"
    SP.CONDUCTORTOKEN -> throwError $ InvalidRequest "ConductorToken identifier is not supported"

  pure APISuccess.Success
  where
    storeAndSendOTP ::
      ( HasFlowEnv m r '["smsCfg" ::: SmsConfig, "kafkaProducerTools" ::: KafkaProducerTools],
        HasField "emailServiceConfig" r Email.Types.EmailServiceConfig,
        CacheFlow m r,
        EsqDBFlow m r,
        EncFlow m r
      ) =>
      Text ->
      SP.IdentifierType ->
      Id Person.Person ->
      Person.Person ->
      SmsConfig ->
      Maybe Text ->
      SOTP.OTPChannel ->
      DRC.RiderConfig ->
      TriggerUpdateAuthOTPReq ->
      Maybe Text ->
      Maybe Text ->
      Maybe Text ->
      m ()
    storeAndSendOTP otpCode' identifierType' personId' person' smsCfg' useFakeOtpM' otpChannel' riderConfig' req' mobileNum countryCode emailVal = do
      otpHash <- getDbHash otpCode'
      let authData =
            AuthData
              { mobileNumber = mobileNum,
                mobileNumberCountryCode = countryCode,
                email = emailVal,
                otp = otpHash
              }
      let redisKey = makeUpdateAuthRedisKey identifierType' (getId personId')
      let expirySeconds = smsCfg'.sessionConfig.authExpiry * 60
      Redis.setExp redisKey authData expirySeconds
      when (isNothing useFakeOtpM') $
        SOTP.sendOTP
          otpChannel'
          otpCode'
          personId'
          person'.merchantId
          person'.merchantOperatingCityId
          req'.mobileCountryCode
          req'.mobileNumber
          req'.email
          riderConfig'.emailOtpConfig
          Nothing

data VerifyUpdateAuthOTPReq = VerifyUpdateAuthOTPReq
  { identifier :: Maybe SP.IdentifierType,
    otp :: Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

verifyUpdateAuthDataOtp ::
  ( CacheFlow m r,
    EsqDBFlow m r,
    EncFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["version" ::: DeploymentVersion]
  ) =>
  (Id Person.Person, Id Merchant.Merchant) ->
  VerifyUpdateAuthOTPReq ->
  m APISuccess.APISuccess
verifyUpdateAuthDataOtp (personId, _merchantId) req = do
  identifierType <- req.identifier & fromMaybeM (InvalidRequest "Identifier type is required")

  let redisKey = makeUpdateAuthRedisKey identifierType (getId personId)
  mbStoredAuthData <- Redis.get redisKey
  storedAuthData :: AuthData <- case mbStoredAuthData of
    Just v -> pure v
    Nothing -> throwError $ InvalidRequest "OTP_EXPIRED_OR_NOT_FOUND: OTP expired or not found"
  reqOtpHash <- getDbHash req.otp
  when ((storedAuthData.otp) /= reqOtpHash) $ throwError $ InvalidRequest "INVALID_OTP: Invalid OTP"

  void $ case identifierType of
    SP.MOBILENUMBER -> do
      storedMobileNumber <- case storedAuthData.mobileNumber of
        Just num -> pure num
        _ -> throwError $ InvalidRequest "AUTH_MOBILE_NOT_FOUND: Mobile number not found in auth data"

      storedCountryCode <- case storedAuthData.mobileNumberCountryCode of
        Just code -> pure code
        _ -> throwError $ InvalidRequest "AUTH_COUNTRY_CODE_NOT_FOUND: Country code not found in auth data"
      encMobileNumber <- encrypt storedMobileNumber
      mobileNumberHash <- getDbHash storedMobileNumber
      QPersonExtra.updateMobileNumberByPersonId personId encMobileNumber mobileNumberHash storedCountryCode
    SP.EMAIL -> do
      storedEmail <- case storedAuthData of
        AuthData {email = Just em} -> pure em
        _ -> throwError $ InvalidRequest "AUTH_EMAIL_NOT_FOUND: Email not found in auth data"
      encryptedValue <- encrypt storedEmail
      QPersonExtra.updateEmailByPersonId personId encryptedValue
    SP.AADHAAR -> throwError $ InvalidRequest "Aadhaar identifier is not supported"
    SP.CONDUCTORTOKEN -> throwError $ InvalidRequest "Conductor identifier is not supported"

  void $ Redis.del redisKey

  pure APISuccess.Success

makeUpdateAuthRedisKey :: SP.IdentifierType -> Text -> Text
makeUpdateAuthRedisKey identifierType identifier =
  "updateAuth:" <> show identifierType <> ":" <> identifier
