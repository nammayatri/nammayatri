{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Action.UI.Notification where

import API.Types.UI.Notification
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Storage.Queries.Notification as SQN
import qualified "lib-dashboard" Storage.Queries.Person as QP
import "lib-dashboard" Tools.Auth
import Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo = T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo)

getListNotification :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.Flow API.Types.UI.Notification.NotificationList
getListNotification tokenInfo mbLimit mbOffset readStatus = do
  list <- case readStatus of
    Just status -> SQN.findByReceiverIdAndReadStatus mbLimit mbOffset (tokenInfo.personId.getId) status
    Nothing -> SQN.findByReceiverId mbLimit mbOffset (tokenInfo.personId.getId)
  let count = length list
      summary = Summary {totalCount = 10000, count = count}
  return $ NotificationList {list = list, summary = summary}

postReadNotification :: TokenInfo -> API.Types.UI.Notification.NotificationReadRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postReadNotification tokenInfo req = do
  isNotificationPresent <- SQN.findByReceiverIdAndId tokenInfo.personId.getId (Kernel.Types.Id.Id $ req.id)
  when (isNothing isNotificationPresent) $ throwError NotificationNotFound
  SQN.updateReadStatusById True (Kernel.Types.Id.Id $ req.id)
  pure Kernel.Types.APISuccess.Success

postUpdateReceiveNotificationStatus :: TokenInfo -> API.Types.UI.Notification.UpdateRecieveNotificationStatusRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postUpdateReceiveNotificationStatus tokenInfo req = do
  transaction <- buildTransaction Safety.UpdateReceiveNotificationStatusEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    QP.updatePersonReceiveNotificationStatus tokenInfo.personId req.readStatus
    pure Kernel.Types.APISuccess.Success
