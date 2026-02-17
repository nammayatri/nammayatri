{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License
 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
-}

module SharedLogic.PersonDefaultEmergencyNumber
  ( DriverEmergencyContactEntity (..),
    getDriverDefaultEmergencyNumbers,
    notifyEmergencyContactsWithKey,
  )
where

import qualified Data.Text as T
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Domain.Types.PersonDefaultEmergencyNumber (DecryptedPersonDefaultEmergencyNumber)
import Environment
import qualified EulerHS.Prelude as EulerHS
import Kernel.Beam.Functions (runInReplica)
import Kernel.External.Encryption (decrypt)
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Safety.Domain.Types.Sos as SafetyDSos
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.PersonDefaultEmergencyNumberExtra as QPDEN
import Tools.Error
import Tools.Notifications hiding (buildTemplate)
import qualified Tools.SMS as Sms

data DriverEmergencyContactEntity = DriverEmergencyContactEntity
  { name :: Text,
    mobileCountryCode :: Text,
    mobileNumber :: Text,
    contactPersonId :: Maybe (Id Person.Person)
  }
  deriving (Generic, Show, Eq)

getDriverDefaultEmergencyNumbers :: (CacheFlow m r, EsqDBFlow m r, EncFlow m r) => Id Person.Person -> m [DriverEmergencyContactEntity]
getDriverDefaultEmergencyNumbers personId = do
  personENList <- runInReplica $ QPDEN.findAllByPersonId personId
  decList <- mapM decrypt personENList
  return $ map toEntity decList
  where
    toEntity :: DecryptedPersonDefaultEmergencyNumber -> DriverEmergencyContactEntity
    toEntity pden =
      DriverEmergencyContactEntity
        { name = pden.name,
          mobileCountryCode = pden.mobileCountryCode,
          mobileNumber = pden.mobileNumber,
          contactPersonId = pden.contactPersonId
        }

data SosNotificationEntityData = SosNotificationEntityData
  { sosId :: Maybe Text,
    notificationKey :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

notifyEmergencyContactsInternal ::
  Person.Person ->
  Notification.Category ->
  Maybe Text ->
  Maybe (Text -> Sms.SendSMSReq) ->
  Bool ->
  [DriverEmergencyContactEntity] ->
  Maybe (Id SafetyDSos.Sos) ->
  (Person.Person -> Flow (Maybe (Text, Text))) ->
  Flow ()
notifyEmergencyContactsInternal person notificationType mbNotificationKey mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId getBodyAndTitle = do
  void $
    mapM_
      ( \emergencyContact ->
          case emergencyContact.contactPersonId of
            Nothing -> sendMessageToContact emergencyContact
            Just emergencyContactId -> do
              contactPersonEntity <- runInReplica $ QPerson.findById emergencyContactId >>= fromMaybeM (PersonNotFound (getId emergencyContactId))
              if contactPersonEntity.merchantOperatingCityId /= person.merchantOperatingCityId
                then sendMessageToContact emergencyContact
                else case contactPersonEntity.deviceToken of
                  Nothing -> sendMessageToContact emergencyContact
                  Just _ -> do
                    mbBodyAndTitle <- getBodyAndTitle contactPersonEntity
                    case mbBodyAndTitle of
                      Just (title, body) -> sendNotificationToEmergencyContact person.id contactPersonEntity body title notificationType mbNotificationKey mbSosId
                      Nothing -> sendMessageToContact emergencyContact
      )
      emergencyContacts
  where
    sendMessageToContact emergencyContact = when useSmsAsBackup $ case mbBuildSmsReq of
      Just buildSmsReq -> sendMessageToEmergencyContact person emergencyContact buildSmsReq
      Nothing -> pure ()

sendNotificationToEmergencyContact ::
  Id Person.Person ->
  Person.Person ->
  Text ->
  Text ->
  Notification.Category ->
  Maybe Text ->
  Maybe (Id SafetyDSos.Sos) ->
  Flow ()
sendNotificationToEmergencyContact senderPersonId recipientPerson body title notificationType _mbNotificationKey mbSosId = do
  let notificationData =
        Notification.NotificationReq
          { category = notificationType,
            subCategory = Nothing,
            showNotification = Notification.SHOW,
            messagePriority = Nothing,
            entity = Notification.Entity Notification.Product senderPersonId.getId (SosNotificationEntityData {sosId = (.getId) <$> mbSosId, notificationKey = _mbNotificationKey}),
            body = body,
            title = title,
            dynamicParams = EmptyDynamicParam,
            auth = Notification.Auth recipientPerson.id.getId ((.getFCMRecipientToken) <$> recipientPerson.deviceToken) Nothing,
            ttl = Nothing,
            sound = Nothing
          }
  runWithServiceConfigForProviders recipientPerson.merchantOperatingCityId recipientPerson.clientId recipientPerson.clientDevice notificationData EulerHS.id (clearDeviceToken recipientPerson.id)

sendMessageToEmergencyContact :: Person.Person -> DriverEmergencyContactEntity -> (Text -> Sms.SendSMSReq) -> Flow ()
sendMessageToEmergencyContact person emergencyContact buildSmsReq = do
  let contactPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
  Sms.sendSMS person.merchantId person.merchantOperatingCityId (buildSmsReq contactPhoneNumber)
    >>= Sms.checkSmsResult

buildTemplate :: [(Text, Text)] -> Text -> Text
buildTemplate paramVars template =
  foldl'
    (\msg (findKey, replaceVal) -> T.replace ("{#" <> findKey <> "#}") replaceVal msg)
    template
    paramVars

fetchNotificationTemplate :: Id DMOC.MerchantOperatingCity -> Text -> Person.Person -> [(Text, Text)] -> Flow (Maybe (Text, Text))
fetchNotificationTemplate merchantOperatingCityId notificationKey recipientPerson templateVariables = do
  mbMerchantPN <- CPN.findMatchingMerchantPN merchantOperatingCityId notificationKey Nothing Nothing recipientPerson.language Nothing
  case mbMerchantPN of
    Just merchantPN -> do
      let title = buildTemplate templateVariables merchantPN.title
          body = buildTemplate templateVariables merchantPN.body
      return $ Just (title, body)
    Nothing -> do
      logError $ "MISSED_FCM - " <> notificationKey <> " for person " <> recipientPerson.id.getId
      return Nothing

notifyEmergencyContactsWithKey ::
  Person.Person ->
  Text ->
  Notification.Category ->
  [(Text, Text)] ->
  Maybe (Text -> Sms.SendSMSReq) ->
  Bool ->
  [DriverEmergencyContactEntity] ->
  Maybe (Id SafetyDSos.Sos) ->
  Flow ()
notifyEmergencyContactsWithKey person notificationKey notificationType templateVariables mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId =
  notifyEmergencyContactsInternal person notificationType (Just notificationKey) mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId $ \contactPersonEntity ->
    fetchNotificationTemplate person.merchantOperatingCityId notificationKey contactPersonEntity templateVariables
