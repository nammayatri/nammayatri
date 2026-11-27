{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.ShuttleInterest
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.ShuttleInterest
import qualified Control.Lens
import qualified Domain.Action.Dashboard.AppManagement.ShuttleInterest
import qualified Domain.Types.Merchant
import qualified "this" Domain.Types.PassOrganization
import qualified "this" Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = "shuttleInterest" :> (GetShuttleInterestList :<|> (GetShuttleInterestMapPoints :<|> (GetShuttleInterestPublishedRoutes :<|> (PostShuttleInterestShuttleRoutesPublish :<|> GetShuttleInterestSubscriptions))))

type GetShuttleInterestList =
  DashboardUserAuth
    ('APP_BACKEND_MANAGEMENT)
    "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_LIST"
    :> API.Types.Dashboard.AppManagement.ShuttleInterest.GetShuttleInterestList

type GetShuttleInterestMapPoints =
  DashboardUserAuth
    ('APP_BACKEND_MANAGEMENT)
    "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_MAP_POINTS"
    :> API.Types.Dashboard.AppManagement.ShuttleInterest.GetShuttleInterestMapPoints

type GetShuttleInterestPublishedRoutes =
  DashboardUserAuth
    ('APP_BACKEND_MANAGEMENT)
    "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_PUBLISHED_ROUTES"
    :> API.Types.Dashboard.AppManagement.ShuttleInterest.GetShuttleInterestPublishedRoutes

type PostShuttleInterestShuttleRoutesPublish =
  DashboardUserAuth
    ('APP_BACKEND_MANAGEMENT)
    "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/POST_SHUTTLE_INTEREST_SHUTTLE_ROUTES_PUBLISH"
    :> API.Types.Dashboard.AppManagement.ShuttleInterest.PostShuttleInterestShuttleRoutesPublish

type GetShuttleInterestSubscriptions =
  DashboardUserAuth
    ('APP_BACKEND_MANAGEMENT)
    "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/GET_SHUTTLE_INTEREST_SUBSCRIPTIONS"
    :> API.Types.Dashboard.AppManagement.ShuttleInterest.GetShuttleInterestSubscriptions

handler :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getShuttleInterestList merchantId city :<|> getShuttleInterestMapPoints merchantId city :<|> getShuttleInterestPublishedRoutes merchantId city :<|> postShuttleInterestShuttleRoutesPublish merchantId city :<|> getShuttleInterestSubscriptions merchantId city

getShuttleInterestList :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleInterestListResp))))))
getShuttleInterestList a7 a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a5 a7 a6
        Tools.ActorInfo.withDashboardUserActorInfo a5 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestList a7 a6 a4 a3 a2 a1
    )

getShuttleInterestMapPoints :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.PassOrganization.PassOrganization) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleInterestMapResp))))
getShuttleInterestMapPoints a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestMapPoints a5 a4 a2 a1
    )

getShuttleInterestPublishedRoutes :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttlePublishedRoutesResp))))
getShuttleInterestPublishedRoutes a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a3 a5 a4
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestPublishedRoutes a5 a4 a2 a1
    )

postShuttleInterestShuttleRoutesPublish :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleRoutesPublishResp)))
postShuttleInterestShuttleRoutesPublish a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/SHUTTLE_INTEREST/POST_SHUTTLE_INTEREST_SHUTTLE_ROUTES_PUBLISH" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.postShuttleInterestShuttleRoutesPublish a4 a3 a1
    )

getShuttleInterestSubscriptions :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleSubscriptionListResp))
getShuttleInterestSubscriptions a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a1 a3 a2
        Tools.ActorInfo.withDashboardUserActorInfo a1 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestSubscriptions a3 a2
    )
