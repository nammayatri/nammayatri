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
    UpdateEmergencySettingsResp,
    EmergencySettingsRes,
    getPersonDetails,
    updatePerson,
    updateDefaultEmergencyNumbers,
    getDefaultEmergencyNumbers,
    updateEmergencySettings,
    getEmergencySettings,
    sendMessageToEmergencyContact,
    notifyEmergencyContacts,
    sendNotificationToEmergencyContact,
  )
where

import Control.Applicative ((<|>))
import qualified Data.HashMap as HM
import Data.List (nubBy)
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as DPDEN
import qualified Domain.Types.Person.PersonDisability as PersonDisability
import Environment
import Kernel.Beam.Functions
import qualified Kernel.Beam.Functions as B
import Kernel.External.Encryption
import qualified Kernel.External.Maps as Maps
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Id
import Kernel.Types.Predicate
import Kernel.Types.Validation
import Kernel.Types.Version
import Kernel.Utils.Common
import qualified Kernel.Utils.Predicates as P
import qualified Kernel.Utils.Text as TU
import Kernel.Utils.Validation
import SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.CachedQueries.Merchant.RiderConfig as QRC
import qualified Storage.Queries.Disability as QD
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Person.PersonDefaultEmergencyNumber as QPersonDEN
import qualified Storage.Queries.Person.PersonDisability as PDisability
import Tools.Error
import Tools.Notifications
import Tools.SMS as Sms

type ProfileRes = Person.PersonAPIEntity

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
    disability :: Maybe Disability,
    hasDisability :: Maybe Bool
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
    priority :: Int,
    enableForFollowing :: Bool
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

getPersonDetails :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m ProfileRes
getPersonDetails (personId, _) = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId
  tag <- case person.hasDisability of
    Just True -> B.runInReplica $ fmap (.tag) <$> PDisability.findByPersonId personId
    _ -> return Nothing
  decPerson <- decrypt person
  return $ Person.makePersonAPIEntity decPerson tag riderConfig

updatePerson :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]) => Id Person.Person -> UpdateProfileReq -> m APISuccess.APISuccess
updatePerson personId req = do
  mPerson <- join <$> QPerson.findByEmail `mapM` req.email
  whenJust mPerson (\_ -> throwError PersonEmailExists)
  mbEncEmail <- encrypt `mapM` req.email

  refCode <- join <$> validateRefferalCode personId `mapM` req.referralCode

  void $
    QPerson.updatePersonalInfo
      personId
      req.firstName
      req.middleName
      req.lastName
      refCode
      mbEncEmail
      req.deviceToken
      req.notificationToken
      req.language
      req.gender
      req.clientVersion
      req.bundleVersion
  updateDisability req.hasDisability req.disability personId

updateDisability :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Maybe Bool -> Maybe Disability -> Id Person.Person -> m APISuccess.APISuccess
updateDisability hasDisability mbDisability personId = do
  case (hasDisability, mbDisability) of
    (Nothing, _) -> logDebug "No Disability"
    (Just False, _) -> do
      QPerson.updateHasDisability personId $ Just False
      PDisability.deleteByPersonId personId
    (Just True, Nothing) -> throwError $ InvalidRequest "Field disability can't be null if hasDisability is True"
    (Just True, Just selectedDisability) -> do
      customerDisability <- B.runInReplica $ PDisability.findByPersonId personId
      QPerson.updateHasDisability personId $ Just True
      let disabilityId = getId $ selectedDisability.id
      disability <- runInReplica $ QD.findByDisabilityId disabilityId >>= fromMaybeM (DisabilityDoesNotExist disabilityId)
      let mbDescription = (selectedDisability.description) <|> (Just disability.description)
      when (isNothing customerDisability) $ do
        newDisability <- makeDisability selectedDisability disability.tag mbDescription
        PDisability.create newDisability
      when (isJust customerDisability) $ do
        PDisability.updateDisabilityByPersonId personId disabilityId disability.tag mbDescription
      where
        makeDisability personDisability tag mbDescription = do
          now <- getCurrentTime
          return $
            PersonDisability.PersonDisability
              { personId = personId,
                disabilityId = getId $ personDisability.id,
                tag = tag,
                description = mbDescription,
                updatedAt = now
              }
  pure APISuccess.Success

validateRefferalCode :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r, HasFlowEnv m r '["internalEndPointHashMap" ::: HM.Map BaseUrl BaseUrl]) => Id Person.Person -> Text -> m (Maybe Text)
validateRefferalCode personId refCode = do
  unless (TU.validateAllDigitWithMinLength 6 refCode) (throwError $ InvalidRequest "Referral Code must have 6 digits")
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId) >>= decrypt
  when (person.hasTakenValidRide) do
    throwError (InvalidRequest "You have been already referred by someone")
  case person.referralCode of
    Just code ->
      if code /= refCode
        then throwError (InvalidRequest "Referral Code is not same")
        else return Nothing -- idempotent behaviour
    Nothing -> do
      merchant <- QMerchant.findById person.merchantId >>= fromMaybeM (MerchantNotFound person.merchantId.getId)
      case (person.mobileNumber, person.mobileCountryCode) of
        (Just mobileNumber, Just countryCode) -> do
          void $ CallBPPInternal.linkReferee merchant.driverOfferApiKey merchant.driverOfferBaseUrl merchant.driverOfferMerchantId refCode mobileNumber countryCode
          return $ Just refCode
        _ -> throwError (PersonMobileNumberIsNULL person.id.getId)

updateDefaultEmergencyNumbers ::
  ( EsqDBFlow m r,
    EncFlow m r,
    CacheFlow m r,
    EsqDBReplicaFlow m r,
    HasFlowEnv m r '["maxEmergencyNumberCount" ::: Int]
  ) =>
  Id Person.Person ->
  Id Merchant.Merchant ->
  UpdateProfileDefaultEmergencyNumbersReq ->
  m UpdateProfileDefaultEmergencyNumbersResp
updateDefaultEmergencyNumbers personId merchantId req = do
  maxEmergencyNumberCount <- asks (.maxEmergencyNumberCount)
  runRequestValidation (validateUpdateProfileDefaultEmergencyNumbersReq maxEmergencyNumberCount) req
  now <- getCurrentTime
  let uniqueRecords = getUniquePersonByMobileNumber req
  newPersonDENList <- buildPersonDefaultEmergencyNumber now `mapM` uniqueRecords
  QPersonDEN.replaceAll personId newPersonDENList
  void $ updateEmergencySettings personId $ updateSettingReq $ not $ null newPersonDENList
  pure APISuccess.Success
  where
    buildPersonDefaultEmergencyNumber now defEmNum = do
      encMobNum <- encrypt defEmNum.mobileNumber
      dbHash <- getDbHash defEmNum.mobileNumber
      mbEmNumPerson <- QPerson.findByMobileNumberAndMerchantId defEmNum.mobileCountryCode dbHash merchantId
      return $
        DPDEN.PersonDefaultEmergencyNumber
          { mobileNumber = encMobNum,
            name = defEmNum.name,
            mobileCountryCode = defEmNum.mobileCountryCode,
            createdAt = now,
            contactPersonId = (.id) <$> mbEmNumPerson,
            enableForFollowing = False,
            priority = defEmNum.priority,
            ..
          }
    updateSettingReq enableEmergencySharing =
      UpdateEmergencySettingsReq
        { shareEmergencyContacts = Just enableEmergencySharing,
          shareTripWithEmergencyContacts = Nothing,
          hasCompletedSafetySetup = Nothing,
          nightSafetyChecks = Nothing
        }

getDefaultEmergencyNumbers :: (CacheFlow m r, EsqDBFlow m r, EsqDBReplicaFlow m r, EncFlow m r) => (Id Person.Person, Id Merchant.Merchant) -> m GetProfileDefaultEmergencyNumbersResp
getDefaultEmergencyNumbers (personId, _) = do
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  return . GetProfileDefaultEmergencyNumbersResp $ DPDEN.makePersonDefaultEmergencyNumberAPIEntity <$> decPersonENList

getUniquePersonByMobileNumber :: UpdateProfileDefaultEmergencyNumbersReq -> [PersonDefaultEmergencyNumber]
getUniquePersonByMobileNumber req =
  nubBy ((==) `on` mobileNumber) req.defaultEmergencyNumbers

data UpdateEmergencySettingsReq = UpdateEmergencySettingsReq
  { shareEmergencyContacts :: Maybe Bool,
    shareTripWithEmergencyContacts :: Maybe Bool,
    nightSafetyChecks :: Maybe Bool,
    hasCompletedSafetySetup :: Maybe Bool
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type UpdateEmergencySettingsResp = APISuccess.APISuccess

updateEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> UpdateEmergencySettingsReq -> m UpdateEmergencySettingsResp
updateEmergencySettings personId req = do
  personENList <- QPersonDEN.findAllByPersonId personId
  let safetySetupCompleted = guard (req.hasCompletedSafetySetup == Just True) >> Just True
  when (fromMaybe False req.shareEmergencyContacts && null personENList) do
    throwError (InvalidRequest "Add atleast one emergency contact.")
  void $
    QPerson.updateEmergencyInfo
      personId
      req.shareEmergencyContacts
      req.shareTripWithEmergencyContacts
      req.nightSafetyChecks
      safetySetupCompleted
  pure APISuccess.Success

data EmergencySettingsRes = EmergencySettingsRes
  { shareEmergencyContacts :: Bool,
    shareTripWithEmergencyContacts :: Bool,
    hasCompletedMockSafetyDrill :: Bool,
    nightSafetyChecks :: Bool,
    hasCompletedSafetySetup :: Bool,
    defaultEmergencyNumbers :: [DPDEN.PersonDefaultEmergencyNumberAPIEntity],
    enablePoliceSupport :: Bool,
    localPoliceNumber :: Maybe Text
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

getEmergencySettings :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> m EmergencySettingsRes
getEmergencySettings personId = do
  person <- runInReplica $ QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  personENList <- runInReplica $ QPersonDEN.findAllByPersonId personId
  decPersonENList <- decrypt `mapM` personENList
  riderConfig <- QRC.findByMerchantOperatingCityId person.merchantOperatingCityId >>= fromMaybeM (RiderConfigDoesNotExist person.merchantOperatingCityId.getId)
  return $
    EmergencySettingsRes
      { shareEmergencyContacts = person.shareEmergencyContacts,
        shareTripWithEmergencyContacts = fromMaybe False person.shareTripWithEmergencyContacts,
        nightSafetyChecks = person.nightSafetyChecks,
        hasCompletedSafetySetup = person.hasCompletedSafetySetup,
        defaultEmergencyNumbers = DPDEN.makePersonDefaultEmergencyNumberAPIEntity <$> decPersonENList,
        enablePoliceSupport = riderConfig.enableLocalPoliceSupport,
        localPoliceNumber = riderConfig.localPoliceNumber,
        hasCompletedMockSafetyDrill = fromMaybe False person.hasCompletedMockSafetyDrill
      }

notifyEmergencyContacts :: Person.Person -> Text -> Text -> Notification.Category -> Maybe Text -> Bool -> Flow ()
notifyEmergencyContacts person body title notificationType message useSmsAsBackup = do
  emergencyContacts <- getDefaultEmergencyNumbers (person.id, person.merchantId)
  void $
    mapM
      ( \emergencyContact ->
          case emergencyContact.contactPersonId of
            Nothing -> sendMessageToContact emergencyContact
            Just emergencyContactId -> do
              contactPersonEntity <- runInReplica $ QPerson.findById emergencyContactId >>= fromMaybeM (PersonNotFound (getId emergencyContactId))
              case contactPersonEntity.deviceToken of
                Nothing -> sendMessageToContact emergencyContact
                Just _ -> sendNotificationToEmergencyContact person body title notificationType
      )
      emergencyContacts.defaultEmergencyNumbers
  where
    sendMessageToContact emergencyContact = when useSmsAsBackup $ case message of
      Just msg -> sendMessageToEmergencyContact person emergencyContact msg
      Nothing -> pure ()

sendNotificationToEmergencyContact :: Person.Person -> Text -> Text -> Notification.Category -> Flow ()
sendNotificationToEmergencyContact person body title notificationType = do
  notifyPerson person.merchantId person.merchantOperatingCityId buildNotificationData
  where
    buildNotificationData =
      Notification.NotificationReq
        { category = notificationType,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product person.id.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing
        }

sendMessageToEmergencyContact :: Person.Person -> DPDEN.PersonDefaultEmergencyNumberAPIEntity -> Text -> Flow ()
sendMessageToEmergencyContact person emergencyContact message = do
  smsCfg <- asks (.smsCfg)
  let sender = smsCfg.sender
      contactPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
  Sms.sendSMS person.merchantId person.merchantOperatingCityId (Sms.SendSMSReq message contactPhoneNumber sender)
    >>= Sms.checkSmsResult
