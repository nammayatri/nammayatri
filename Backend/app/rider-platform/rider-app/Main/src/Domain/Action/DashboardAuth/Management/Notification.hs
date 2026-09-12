{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for direct-dashboard routes whose request needs more
-- than the verified operator's id or name: fleet-owner resolution, fleet-owner
-- verification, dashboard-database writes after the call, and similar.
--
-- provider-dashboard did this work in its own hand-written
-- @Domain.Action.ProviderPlatform.*@ layer before forwarding the call. The
-- generated @API.Action.DashboardAuth@ handler calls these functions instead of
-- the domain handler for every endpoint marked @appServerHandler: custom@ in
-- the spec, so this logic lives here and is never overwritten by the generator.
module Domain.Action.DashboardAuth.Management.Notification
  ( getNotificationNotificationList,
    postNotificationNotificationRespond,
  )
where

import qualified API.Types.RiderPlatform.Management.Notification
import qualified DashboardAlert.Domain.Action.Dashboard.List
import qualified Domain.Action.Dashboard.Notification
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getNotificationNotificationList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.Flow DashboardAlert.Domain.Action.Dashboard.List.NotificationListResp)
getNotificationNotificationList a5 a4 a3 a2 a1 = do
  let fleetOwnerIds = []
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a3
  Domain.Action.Dashboard.Notification.getNotificationNotificationList a5 a4 topic a2 a1

postNotificationNotificationRespond :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.RiderPlatform.Management.Notification.RespondReq -> Environment.Flow Kernel.Types.APISuccess.APISuccess)
postNotificationNotificationRespond a4 a3 a2 a1 = do
  let fleetOwnerIds = []
  topic <- Tools.Auth.DashboardUserAuth.resolveRequestorTopic fleetOwnerIds a2
  Domain.Action.Dashboard.Notification.postNotificationNotificationRespond a4 a3 topic a1
