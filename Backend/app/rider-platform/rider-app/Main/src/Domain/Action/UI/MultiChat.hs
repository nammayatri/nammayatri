{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.MultiChat where

import qualified API.Types.UI.MultiChat
import Data.OpenApi (ToSchema)
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
import Servant
import qualified Storage.Queries.Person as QPerson
import Tools.Auth
import Tools.Notifications

postMultichatMessage ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.MultiChat.MultiChatReq ->
    Environment.Flow APISuccess.APISuccess
  )
postMultichatMessage (_, _) (API.Types.UI.MultiChat.MultiChatReq {..}) = do
  person <- runInReplica $ QPerson.findById chatPersonId >>= fromMaybeM (PersonNotFound (getId chatPersonId))
  when (isJust person.deviceToken) $ notifyPerson person.merchantId person.merchantOperatingCityId person.id (buildNotificationData person)
  return APISuccess.Success
  where
    buildNotificationData person =
      Notification.NotificationReq
        { category = Notification.MULTICHAT_MESSAGE,
          subCategory = Nothing,
          showNotification = Notification.SHOW,
          messagePriority = Nothing,
          entity = Notification.Entity Notification.Product person.id.getId (),
          body = body,
          title = title,
          dynamicParams = EmptyDynamicParam,
          auth = Notification.Auth person.id.getId person.deviceToken person.notificationToken,
          ttl = Nothing,
          sound = Just "default"
        }
