{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PersonDefaultEmergencyNumber where

import qualified Domain.Action.UI.PersonDefaultEmergencyNumber as DPDEN
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.MerchantOperatingCity as MerchantOperatingCity
import qualified Domain.Types.Person as Person
import qualified Domain.Types.Sos as DSos
import Environment
import Kernel.Beam.Functions
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.NotificationSoundsConfig as SQNSC
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications
import Tools.SMS as Sms

data SosNotificationEntityData = SosNotificationEntityData
  { sosId :: Maybe Text
  }
  deriving (Generic, Show, Eq, ToJSON, FromJSON)

notifyEmergencyContactsInternal :: Person.Person -> Notification.Category -> Maybe (Text -> Sms.SendSMSReq) -> Bool -> [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Maybe (Id DSos.Sos) -> (Person.Person -> Flow (Maybe (Text, Text))) -> Flow ()
notifyEmergencyContactsInternal person notificationType mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId getBodyAndTitle = do
  void $
    mapM
      ( \emergencyContact ->
          case emergencyContact.contactPersonId of
            Nothing -> sendMessageToContact emergencyContact
            Just emergencyContactId -> do
              contactPersonEntity <- runInReplica $ QPerson.findById emergencyContactId >>= fromMaybeM (PersonNotFound (getId emergencyContactId))
              case contactPersonEntity.deviceToken of
                Nothing -> sendMessageToContact emergencyContact
                Just _ -> do
                  mbBodyAndTitle <- getBodyAndTitle contactPersonEntity
                  case mbBodyAndTitle of
                    Just (title, body) -> sendNotificationToEmergencyContact person.id contactPersonEntity body title notificationType mbSosId
                    Nothing -> sendMessageToContact emergencyContact
      )
      emergencyContacts
  where
    sendMessageToContact emergencyContact = when useSmsAsBackup $ case mbBuildSmsReq of
      Just buildSmsReq -> sendMessageToEmergencyContact person emergencyContact buildSmsReq
      Nothing -> pure ()

notifyEmergencyContacts :: Person.Person -> Text -> Text -> Notification.Category -> Maybe (Text -> Sms.SendSMSReq) -> Bool -> [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Maybe (Id DSos.Sos) -> Flow ()
notifyEmergencyContacts person body title notificationType mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId =
  notifyEmergencyContactsInternal person notificationType mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId $ \_ -> return $ Just (body, title)

sendNotificationToEmergencyContact :: Id Person.Person -> Person.Person -> Text -> Text -> Notification.Category -> Maybe (Id DSos.Sos) -> Flow ()
sendNotificationToEmergencyContact senderPersonId recipientPerson body title notificationType mbSosId = do
  notificationSoundFromConfig <- SQNSC.findByNotificationType notificationType recipientPerson.merchantOperatingCityId
  disabilityTag <- getDisabilityTag recipientPerson.hasDisability recipientPerson.id
  notificationSound <- getNotificationSound disabilityTag notificationSoundFromConfig
  notifyPerson recipientPerson.merchantId recipientPerson.merchantOperatingCityId recipientPerson.id (buildNotificationData notificationSound) Nothing
  where
    buildNotificationData notificationSound =
      Notification.NotificationReq
        { category = notificationType,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product senderPersonId.getId (SosNotificationEntityData {sosId = (.getId) <$> mbSosId}),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth recipientPerson.id.getId recipientPerson.deviceToken recipientPerson.notificationToken,
          ttl = Nothing,
          sound = notificationSound
        }

sendMessageToEmergencyContact :: Person.Person -> DPDEN.PersonDefaultEmergencyNumberAPIEntity -> (Text -> Sms.SendSMSReq) -> Flow ()
sendMessageToEmergencyContact person emergencyContact buildSmsReq = do
  let contactPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
  Sms.sendSMS person.merchantId person.merchantOperatingCityId (buildSmsReq contactPhoneNumber)
    >>= Sms.checkSmsResult

fetchNotificationTemplate :: Id MerchantOperatingCity.MerchantOperatingCity -> Text -> Person.Person -> [(Text, Text)] -> Flow (Maybe (Text, Text))
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

notifyEmergencyContactsWithKey :: Person.Person -> Text -> Notification.Category -> [(Text, Text)] -> Maybe (Text -> Sms.SendSMSReq) -> Bool -> [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Maybe (Id DSos.Sos) -> Flow ()
notifyEmergencyContactsWithKey person notificationKey notificationType templateVariables mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId =
  notifyEmergencyContactsInternal person notificationType mbBuildSmsReq useSmsAsBackup emergencyContacts mbSosId $ \contactPersonEntity ->
    fetchNotificationTemplate person.merchantOperatingCityId notificationKey contactPersonEntity templateVariables
