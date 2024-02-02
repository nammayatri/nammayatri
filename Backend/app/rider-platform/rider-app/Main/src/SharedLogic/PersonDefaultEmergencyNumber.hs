{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.PersonDefaultEmergencyNumber where

import qualified Domain.Types.Person as Person
import qualified Domain.Types.Person.PersonDefaultEmergencyNumber as DPDEN
import Environment
import Kernel.Beam.Functions
import qualified Kernel.External.Notification as Notification
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications
import Tools.SMS as Sms

notifyEmergencyContacts :: Person.Person -> Text -> Text -> Notification.Category -> Maybe Text -> Bool -> [DPDEN.PersonDefaultEmergencyNumberAPIEntity] -> Flow ()
notifyEmergencyContacts person body title notificationType message useSmsAsBackup emergencyContacts = do
  void $
    mapM
      ( \emergencyContact ->
          case emergencyContact.contactPersonId of
            Nothing -> sendMessageToContact emergencyContact
            Just emergencyContactId -> do
              contactPersonEntity <- runInReplica $ QPerson.findById emergencyContactId >>= fromMaybeM (PersonNotFound (getId emergencyContactId))
              case contactPersonEntity.deviceToken of
                Nothing -> sendMessageToContact emergencyContact
                Just _ -> sendNotificationToEmergencyContact contactPersonEntity body title notificationType
      )
      emergencyContacts
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
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken
        }

sendMessageToEmergencyContact :: Person.Person -> DPDEN.PersonDefaultEmergencyNumberAPIEntity -> Text -> Flow ()
sendMessageToEmergencyContact person emergencyContact message = do
  smsCfg <- asks (.smsCfg)
  let sender = smsCfg.sender
      contactPhoneNumber = emergencyContact.mobileCountryCode <> emergencyContact.mobileNumber
  Sms.sendSMS person.merchantId person.merchantOperatingCityId (Sms.SendSMSReq message contactPhoneNumber sender)
    >>= Sms.checkSmsResult
