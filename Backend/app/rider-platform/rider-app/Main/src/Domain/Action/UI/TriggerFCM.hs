module Domain.Action.UI.TriggerFCM (postTriggerFCMMessage) where

import qualified API.Types.UI.TriggerFCM
import Domain.Types.EmptyDynamicParam
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import Kernel.Beam.Functions
import qualified Kernel.External.Notification as Notification
import Kernel.External.Notification.Interface.Types
import Kernel.Prelude
import qualified Kernel.Types.APISuccess as APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.Person as QPerson
import Tools.Error (CustomerError (..))
import Tools.Notifications

postTriggerFCMMessage ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.TriggerFCM.TriggerFcmReq ->
    Environment.Flow APISuccess.APISuccess
  )
postTriggerFCMMessage (mbPersonId, _) (API.Types.UI.TriggerFCM.TriggerFcmReq {..}) = do
  senderId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  person <- runInReplica $ QPerson.findById chatPersonId >>= fromMaybeM (PersonNotFound (getId chatPersonId))
  case person.deviceToken of
    Nothing -> throwError DeviceTokenNotFound
    Just _ -> do
      notifyPerson person.merchantId person.merchantOperatingCityId person.id (buildNotificationData person senderId) Nothing
  return APISuccess.Success
  where
    buildNotificationData person senderId =
      Notification.NotificationReq
        { category = Notification.TRIGGER_FCM,
          subCategory = Nothing,
          showNotification = if showNotification == Just False then Notification.DO_NOT_SHOW else Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product person.id.getId (buildFCMEntityData senderId),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = Just "default"
        }

    buildFCMEntityData senderId =
      API.Types.UI.TriggerFCM.FCMEntityData
        { channelId = channelId,
          personId = senderId,
          source = source
        }
