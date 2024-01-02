{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.Notification where

import API.Types.UI.Notification
import qualified API.Types.UI.Notification
import qualified "dashboard-helper-api" Dashboard.SafetyPlatform as Safety
import Data.OpenApi (ToSchema)
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Domain.Types.Person
import qualified Domain.Types.Transaction as DT
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant hiding (throwError)
import qualified SharedLogic.Transaction as T
import Storage.Beam.CommonInstances ()
import Storage.Queries.Notification as SQN
import "lib-dashboard" Tools.Auth
import Tools.Error
import "lib-dashboard" Tools.Error

buildTransaction ::
  ( MonadFlow m
  ) =>
  Safety.SafetyEndpoint ->
  TokenInfo ->
  Text ->
  m DT.Transaction
buildTransaction endpoint tokenInfo request =
  T.buildTransactionForSafetyDashboard (DT.SafetyAPI endpoint) (Just tokenInfo) request

getListNotification :: TokenInfo -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow API.Types.UI.Notification.NotificationList
getListNotification tokenInfo mbLimit mbOffset = do
  list <- SQN.findByReceiverId mbLimit mbOffset (tokenInfo.personId.getId)
  let count = length list
      summary = Summary {totalCount = 10000, count = count}
  return $ NotificationList {list = list, summary = summary}

postReadNotification :: TokenInfo -> API.Types.UI.Notification.NotificationReadRequest -> Environment.Flow Kernel.Types.APISuccess.APISuccess
postReadNotification tokenInfo req = do
  transaction <- buildTransaction Safety.ReadNotificationEndpoint tokenInfo (encodeToText req)
  T.withTransactionStoring transaction $ do
    isNotificationPresent <- SQN.findByReceiverIdAndId tokenInfo.personId.getId (Kernel.Types.Id.Id $ req.id)
    when (isNothing isNotificationPresent) $ throwError NotificationNotFound
    SQN.updateReadStatusById True (Kernel.Types.Id.Id $ req.id)
    pure Kernel.Types.APISuccess.Success
