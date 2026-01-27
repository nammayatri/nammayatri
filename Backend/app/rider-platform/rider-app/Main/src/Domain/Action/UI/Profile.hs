{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.Profile
  ( ProfileRes,
    UpdateProfileReq (..),
    UpdateProfileResp,
    UpdateProfileDefaultEmergencyNumbersReq (..),
    PersonDefaultEmergencyNumber (..),
    UpdateProfileDefaultEmergencyNumbersResp,
    GetProfileDefaultEmergencyNumbersResp (..),
    UpdateEmergencySettingsReq (..),
    MarketEventReq (..),
    UpdateEmergencySettingsResp,
    EmergencySettingsRes,
    getPersonDetails,
    updatePerson,
    updateDefaultEmergencyNumbers,
    getDefaultEmergencyNumbers,
    updateEmergencySettings,
    getEmergencySettings,
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
import Data.List (nubBy)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Action.UI.Registration as DR
import Domain.Types.Booking as DBooking
import qualified Domain.Types.ClientPersonInfo as DCP
import qualified Domain.Types.Extra.MerchantPaymentMethod as DMPM
import qualified Domain.Types.IntegratedBPPConfig as DIBC
import qualified Domain.Types.Merchant as Merchant
import Domain.Types.Person (RideShareOptions)
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person as SP
import qualified Domain.Types.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Types.PersonDisability as PersonDisability
import qualified Domain.Types.PersonFlowStatus as DPFS
import qualified Domain.Types.RiderConfig as DRC
import Domain.Types.SafetySettings
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
import Kernel.Types.Validation
import Kernel.Types.Version
import Kernel.Utils.Common
import Kernel.External.Types (SchedulerFlow, SchedulerType)
import qualified Kernel.Utils.Predicates as P
import Kernel.Utils.Validation
import Kernel.Utils.Version
import Lib.SessionizerMetrics.Types.Event
import qualified Lib.Yudhishthira.Tools.Utils as YUtils
import qualified Lib.Yudhishthira.Types as LYT
import qualified SharedLogic.BehaviourManagement.CustomerCancellationRate as CCR
import SharedLogic.Cac
import qualified SharedLogic.IntegratedBPPConfig as SIBC
import qualified SharedLogic.MessageBuilder as MessageBuilder
import qualified SharedLogic.OTP as SOTP
import SharedLogic.Person as SLP
import SharedLogic.PersonDefaultEmergencyNumber as SPDEN
import qualified SharedLogic.Referral as Referral
import qualified Storage.CachedQueries.Merchant.PayoutConfig as CPC
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.CachedQueries.OTPRest.OTPRest as OTPRest
import qualified Storage.CachedQueries.Person.PersonFlowStatus as QPFS
import Storage.Queries.Booking as QBooking
import qualified Storage.Queries.ClientPersonInfo as QCP
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Storage.Queries.PersonDisability as PDisability
import qualified Storage.Queries.PersonExtra as QPersonExtra
import qualified Storage.Queries.PersonStats as QPersonStats
import qualified Storage.Queries.SafetySettings as QSafety
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
    paymentMode :: Maybe DMPM.PaymentMode
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
    paymentMode :: Maybe DMPM.PaymentMode
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

newtype UpdateProfileDefaultEmergencyNumbersReq = UpdateProfileDefaultEmergencyNumbersReq
  { defaultEmergencyNumbers :: [PersonDefaultEmergencyNumber]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validateUpdateProfileDefaultEmergencyNumbersReq :: Int -> Validate UpdateProfileDefaultEmergencyNumbersReq
validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount UpdateProfileDefaultEmergencyNumbersReq {..} =
  sequenceA_
    [ validateList "defaultEmergencyNumbers" defaultEmergencyNumbers validatePersonDefaultEmergencyNumber,
      validateField "defaultEmergencyNumbers" defaultEmergencyNumbers $ MaxLength maxEmergencyNumberCount
    ]

data PersonDefaultEmergencyNumber = PersonDefaultEmergencyNumber
  { name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    priority :: Maybe Int,
    enableForFollowing :: Maybe Bool,
    shareTripWithEmergencyContactOption :: Maybe RideShareOptions
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

validatePersonDefaultEmergencyNumber :: Validate PersonDefaultEmergencyNumber
validatePersonDefaultEmergencyNumber PersonDefaultEmergencyNumber {..} =
  sequenceA_
    [ validateField "mobileNumber" mobileNumber P.mobileNumber,
      validateField "mobileCountryCode" mobileCountryCode P.mobileCountryCode
    ]

type UpdateProfileDefaultEmergencyNumbersResp = APISuccess.APISuccess

newtype GetProfileDefaultEmergencyNumbersResp = GetProfileDefaultEmergencyNumbersResp
  { defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity]
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

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
  m ProfileRes
getPersonDetails (personId, _) toss tenant' context includeProfileImage mbBundleVersion mbRnVersion mbClientVersion mbClientConfigVersion mbDevice = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  decPerson <- decrypt person
  personStats <- QPersonStats.findByPersonId personId >>= fromMaybeM (PersonStatsNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  let device = getDeviceFromText mbDevice
      totalRides = personStats.completedRides + personStats.driverCancelledRides + personStats.userCancelledRides
      rate = (personStats.userCancelledRides * 100) `div` max 1 totalRides
      sendCancellationRate = totalRides >= (fromMaybe 1000 riderConfig.minRidesToShowCancellationRate) && personStats.isBackfilled == Just True
  let cancellationPerc = if sendCancellationRate then Just rate else Nothing
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PDisability.findByPersonId personId
    _ -> return Nothing

  when ((decPerson.clientBundleVersion /= mbBundleVersion || decPerson.clientSdkVersion /= mbClientVersion || decPerson.clientConfigVersion /= mbClientConfigVersion || decPerson.clientReactNativeVersion /= mbRnVersion || decPerson.clientDevice /= device) && isJust device) do
    cloudType <- asks (.cloudType)
    deploymentVersion <- asks (.version)
    void $ QPerson.updatePersonVersions person mbBundleVersion mbClientVersion mbClientConfigVersion device deploymentVersion.getDeploymentVersion mbRnVersion cloudType
  when (isJust decPerson.email && not (isValidEmail decPerson.email)) do
    logDebug $ "Invalid email, updating person email to nothing , Previous emailId: " <> show decPerson.email <> " for person id " <> show personId
    let updatedPerson = person {Person.email = Nothing}
    void $ QPerson.updateByPrimaryKey updatedPerson
  systemConfigs <- L.getOption KBT.Tables
  let useCACConfig = maybe False (.useCACForFrontend) systemConfigs
  let context' = fromMaybe DAKM.empty (DA.decode $ BSL.pack $ T.unpack $ fromMaybe "{}" context)
  frntndfgs <- if useCACConfig then getFrontendConfigs person toss tenant' context' else return $ Just DAKM.empty
  let mbMd5Digest = T.pack . show . MD5.md5 . DA.encode <$> frntndfgs
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  isSafetyCenterDisabled_ <- SLP.checkSafetyCenterDisabled person safetySettings
  hasTakenValidRide <- QCP.findAllByPersonId personId
  let hasTakenValidFirstCabRide = validRideCount hasTakenValidRide BecknEnums.CAB
      hasTakenValidFirstAutoRide = validRideCount hasTakenValidRide BecknEnums.AUTO_RICKSHAW
      hasTakenValidFirstBikeRide = validRideCount hasTakenValidRide BecknEnums.MOTORCYCLE
      hasTakenValidAmbulanceRide = validRideCount hasTakenValidRide BecknEnums.AMBULANCE
      hasTakenValidTruckRide = validRideCount hasTakenValidRide BecknEnums.TRUCK
      hasTakenValidBusRide = validRideCount hasTakenValidRide BecknEnums.BUS
  newCustomerReferralCode <-
    if isNothing person.customerReferralCode
      then do
        newCustomerReferralCode <- DR.generateCustomerReferralCode
        checkIfReferralCodeExists <- QPerson.findPersonByCustomerReferralCode (Just newCustomerReferralCode)
        if isNothing checkIfReferralCodeExists
          then do
            void $ QPerson.updateCustomerReferralCode personId newCustomerReferralCode
            pure $ Just newCustomerReferralCode
          else pure Nothing
      else pure person.customerReferralCode
  mbPayoutConfig <- CPC.findByCityIdAndVehicleCategory person.merchantOperatingCityId VehicleCategory.AUTO_CATEGORY Nothing
  let vehicleTypes = [Enums.BUS, Enums.METRO, Enums.SUBWAY]
  integratedBPPConfigs <-
    concatMapM
      ( \vType ->
          SIBC.findAllIntegratedBPPConfig person.merchantOperatingCityId vType DIBC.MULTIMODAL
      )
      vehicleTypes
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
  fork "Check customer cancellation rate blocking" $ do
    personFlowStatus <- QPFS.getStatus personId
    let isNotOnRide = case personFlowStatus of
          Just DPFS.IDLE -> True
          Just (DPFS.FEEDBACK_PENDING _) -> True
          _ -> False
    when isNotOnRide $ do
      CCR.nudgeOrBlockCustomer riderConfig person
  makeProfileRes riderConfig decPerson tag mbMd5Digest isSafetyCenterDisabled_ newCustomerReferralCode hasTakenValidFirstCabRide hasTakenValidFirstAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide hasTakenValidTruckRide hasTakenValidBusRide safetySettings personStats cancellationPerc mbPayoutConfig integratedBPPConfigs isMultimodalRider includeProfileImage
  where
    makeProfileRes riderConfig Person.Person {..} disability md5DigestHash isSafetyCenterDisabled_ newCustomerReferralCode hasTakenCabRide hasTakenAutoRide hasTakenValidFirstBikeRide hasTakenValidAmbulanceRide hasTakenValidTruckRide hasTakenValidBusRide safetySettings personStats cancellationPerc mbPayoutConfig integratedBPPConfigs isMultimodalRider includeProfileImageParam = do
      gtfsVersion <-
        withTryCatch "getGtfsVersion:getPersonDetails" (mapM OTPRest.getGtfsVersion integratedBPPConfigs) >>= \case
          Left _ -> return (map (.feedKey) integratedBPPConfigs)
          Right gtfsVersions -> return gtfsVersions
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

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, EventStreamFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.HashMap BaseUrl BaseUrl, "version" ::: DeploymentVersion], HasFlowEnv m r '["kafkaProducerTools" ::: KafkaProducerTools]) => Id Person.Person -> Id Merchant.Merchant -> UpdateProfileReq -> Maybe Text -> Maybe Version -> Maybe Version -> Maybe Version -> Maybe Text -> m APISuccess.APISuccess
updatePerson personId merchantId req mbRnVersion mbBundleVersion mbClientVersion mbClientConfigVersion mbDevice = do
  mPerson <- join <$> QPerson.findByEmailAndMerchantId merchantId `mapM` req.email
  whenJust mPerson (\person -> when (person.id /= personId) $ throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email
  mbEncBusinessEmail <- encrypt `mapM` req.businessEmail
  deploymentVersion <- asks (.version)
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
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
  updateDisability req.hasDisability req.disability personId

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

updateDefaultEmergencyNumbers ::
  Id Person.Person ->
  Id Merchant.Merchant ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  Flow UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId merchantId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  oldPersonDENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  let uniqueRecords = getUniquePersonByMobileNumber req
  shareTripOptionDefault <- case any (\x -> isNothing x.shareTripWithEmergencyContactOption) req.defaultEmergencyNumbers of
    True -> do
      person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
      return person.shareTripWithEmergencyContactOption
    False -> pure Nothing
  newPersonDENList <- buildPersonDefaultEmergencyNumber now shareTripOptionDefault `mapM` uniqueRecords
  let updatedWithAggregatedRideShareSetting = QSafety.emptyUpdateEmergencyInfo {QSafety.aggregatedRideShare = Just $ getAggregatedRideShareSetting req.defaultEmergencyNumbers}
  void $ QSafety.upsert personId updatedWithAggregatedRideShareSetting
  fork "Send Emergency Contact Added Message" $ do
    sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList
  QPersonDEN.replaceAll personId newPersonDENList
  pure APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now shareTripOptionDefault defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      dbHash <- getDbHash defEmNum.mobileNumber
      mbEmNumPerson <- QPerson.findByMobileNumberAndMerchantId defEmNum.mobileCountryCode dbHash merchantId
      let shareTripWithEmergencyContactOption = case defEmNum.shareTripWithEmergencyContactOption of
            Just x -> Just x
            Nothing ->
              if defEmNum.enableForFollowing == Just True
                then shareTripOptionDefault
                else Just Person.NEVER_SHARE
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            contactPersonId = (.id) <$> mbEmNumPerson,
            enableForFollowing = fromMaybe False defEmNum.enableForFollowing,
            enableForShareRide = False,
            priority = fromMaybe 1 defEmNum.priority,
            merchantId = Just merchantId,
            ..
          }

    getAggregatedRideShareSetting :: [PersonDefaultEmergencyNumber] -> Person.RideShareOptions
    getAggregatedRideShareSetting [] = Person.NEVER_SHARE
    getAggregatedRideShareSetting (x : xs) =
      case x.shareTripWithEmergencyContactOption of
        Just Person.ALWAYS_SHARE -> Person.ALWAYS_SHARE
        Just Person.SHARE_WITH_TIME_CONSTRAINTS ->
          let nextResult = getAggregatedRideShareSetting xs
           in if nextResult == Person.ALWAYS_SHARE then nextResult else Person.SHARE_WITH_TIME_CONSTRAINTS
        Just Person.NEVER_SHARE -> getAggregatedRideShareSetting xs
        Nothing -> getAggregatedRideShareSetting xs

sendEmergencyContactAddedMessage :: Id Person.Person -> [DPDEN.PersonDefaultEmergencyNumber] -> [DPDEN.PersonDefaultEmergencyNumber] -> Flow ()
sendEmergencyContactAddedMessage personId newPersonDENList oldPersonDENList = do
  decNew <- decrypt `mapM` newPersonDENList
  decOld <- decrypt `mapM` oldPersonDENList
  let oldList = (.mobileNumber) <$> decOld
      newList = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decNew
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  buildSmsReq <-
    MessageBuilder.buildAddedAsEmergencyContactMessage person.merchantOperatingCityId $
      MessageBuilder.BuildAddedAsEmergencyContactMessageReq
        { userName = SLP.getName person,
          appUrl = riderConfig.appUrl
        }
  let filterNewContacts personDEN = return $ not $ personDEN.mobileNumber `elem` oldList
  newlyAddedContacts <- filterM filterNewContacts newList
  SPDEN.notifyEmergencyContacts person (notificationMessage person) "Emergency Contact Added" Notification.EMERGENCY_CONTACT_ADDED (Just buildSmsReq) riderConfig.enableEmergencyContactAddedMessage newlyAddedContacts Nothing
  where
    notificationMessage person = SLP.getName person <> " has added you as the emergency contact."

getDefaultEmergencyNumbers :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers (personId, _) = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  emergencyContactsEntity <- mapM makeAPIEntityAndCheckOnRide decPersonENList
  return $ GetProfileDefaultEmergencyNumbersResp emergencyContactsEntity
  where
    makeAPIEntityAndCheckOnRide :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => DPDEN.DecryptedPersonDefaultEmergencyNumber -> m DPDEN.PersonDefaultEmergencyNumberAPIEntity
    makeAPIEntityAndCheckOnRide personEN = do
      onRide <- case (personEN.contactPersonId, personEN.priority == 0) of
        (Just id, True) -> do
          activeBookings <- runInReplica $ QBooking.findByRiderIdAndStatus id DBooking.activeBookingStatus
          return $ not $ null activeBookings
        _ -> return False
      return $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity onRide personEN

getUniquePersonByMobileNumber :: UpdateProfileDefaultEmergencyNumbersReq -> [PersonDefaultEmergencyNumber]
getUniquePersonByMobileNumber req =
  nubBy ((==) `on` (.mobileNumber)) req.defaultEmergencyNumbers

data UpdateEmergencySettingsReq = UpdateEmergencySettingsReq
  { shareEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContactOption :: Maybe Person.RideShareOptions,
    nightSafetyChecks :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool,
    autoCallDefaultContact :: Maybe Bool,
    enablePostRideSafetyCheck :: Maybe Person.RideShareOptions,
    enableUnexpectedEventsCheck :: Maybe Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Maybe Bool,
    informPoliceSos :: Maybe Bool,
    notifySafetyTeamForSafetyCheckFailure :: Maybe Bool,
    notifySosWithEmergencyContacts :: Maybe Bool,
    shakeToActivate :: Maybe Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    enableOtpLessRide :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateEmergencySettingsResp = APISuccess.APISuccess

updateEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateEmergencySettingsReq -> m UpdateEmergencySettingsResp
updateEmergencySettings personId req = do
  personENList <- QPersonDEN.findAllByPersonId personId
  let shareTripOptions = case req.shareTripWithEmergencyContactOption of
        Nothing ->
          case req.shareTripWithEmergencyContacts of
            Just True -> Just Person.SHARE_WITH_TIME_CONSTRAINTS
            Just False -> Just Person.NEVER_SHARE
            Nothing -> Just Person.ALWAYS_SHARE
        _ -> req.shareTripWithEmergencyContactOption
  when (fromMaybe False req.shareEmergencyContacts && null personENList) do
    throwError (InvalidRequest "Add atleast one emergency contact.")
  void $ updateSafetySettings req
  when updateShareOptionForEmergencyContacts $ QPersonDEN.updateShareTripWithEmergencyContactOptions personId shareTripOptions
  pure APISuccess.Success
  where
    updateSafetySettings :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => UpdateEmergencySettingsReq -> m ()
    updateSafetySettings UpdateEmergencySettingsReq {..} = do
      let shareContacts = fromMaybe False shareEmergencyContacts
          setContactField field = bool (Just shareContacts) field (isJust field)
          emergencyInfo =
            QSafety.UpdateEmergencyInfo
              { autoCallDefaultContact = setContactField autoCallDefaultContact,
                notifySosWithEmergencyContacts = setContactField notifySosWithEmergencyContacts,
                aggregatedRideShare = guard updateShareOptionForEmergencyContacts >> shareTripWithEmergencyContactOption,
                ..
              }
      void $ QSafety.upsert personId emergencyInfo

    updateShareOptionForEmergencyContacts = isJust req.shareTripWithEmergencyContactOption || isJust req.shareTripWithEmergencyContacts

data EmergencySettingsRes = EmergencySettingsRes
  { shareEmergencyContacts :: Bool,
    shareTripWithEmergencyContacts :: Bool,
    shareTripWithEmergencyContactOption :: Person.RideShareOptions,
    hasCompletedMockSafetyDrill :: Bool,
    nightSafetyChecks :: Bool,
    hasCompletedSafetySetup :: Bool,
    defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity],
    enablePoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text,
    autoCallDefaultContact :: Bool,
    notifySosWithEmergencyContacts :: Bool,
    enablePostRideSafetyCheck :: Person.RideShareOptions,
    enableUnexpectedEventsCheck :: Person.RideShareOptions,
    informPoliceSos :: Bool,
    notifySafetyTeamForSafetyCheckFailure :: Bool,
    shakeToActivate :: Bool,
    safetyCenterDisabledOnDate :: Maybe UTCTime,
    enableOtpLessRide :: Maybe Bool,
    safetyCheckStartTime :: Seconds,
    safetyCheckEndTime :: Seconds
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> m EmergencySettingsRes
getEmergencySettings personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personENList <- QPersonDEN.findpersonENListWithFallBack personId (Just person)
  safetySettings <- QSafety.findSafetySettingsWithFallback personId (Just person)
  let SafetySettings {personId = _id, ..} = safetySettings
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  decPersonENList <- decrypt `mapM` personENList
  return $
    EmergencySettingsRes
      { shareTripWithEmergencyContacts = any (\x -> (x.shareTripWithEmergencyContactOption /= Just Person.NEVER_SHARE) && isJust x.shareTripWithEmergencyContactOption) personENList,
        shareTripWithEmergencyContactOption = fromMaybe Person.NEVER_SHARE (listToMaybe personENList >>= (.shareTripWithEmergencyContactOption)),
        defaultEmergencyNumbers = DPDEN.makePersonDefaultEmergencyNumberAPIEntity False <$> decPersonENList,
        enablePoliceSupport = riderConfig.enableLocalPoliceSupport,
        localPoliceNumber = riderConfig.localPoliceNumber,
        hasCompletedMockSafetyDrill = fromMaybe False safetySettings.hasCompletedMockSafetyDrill,
        shareEmergencyContacts = safetySettings.notifySosWithEmergencyContacts,
        safetyCheckEndTime = riderConfig.safetyCheckEndTime,
        safetyCheckStartTime = riderConfig.safetyCheckStartTime,
        ..
      }

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
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId Nothing >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)

  identifierType <- req.identifier & fromMaybeM (InvalidRequest "Identifier type is required")
  let useFakeOtpM = (show <$> useFakeSms smsCfg) <|> person.useFakeOtp

  generatedOtpCode <- maybe generateOTPCode return useFakeOtpM
  let otpChannel = case identifierType of
        SP.MOBILENUMBER -> SOTP.SMS
        SP.EMAIL -> SOTP.EMAIL
        SP.AADHAAR -> SOTP.SMS

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

  void $ Redis.del redisKey

  pure APISuccess.Success

makeUpdateAuthRedisKey :: SP.IdentifierType -> Text -> Text
makeUpdateAuthRedisKey identifierType identifier =
  "updateAuth:" <> show identifierType <> ":" <> identifier
