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

type API = "shuttleInterest" :> (GetShuttleInterestList :<|> GetShuttleInterestMapPoints)

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

handler :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getShuttleInterestList merchantId city :<|> getShuttleInterestMapPoints merchantId city

getShuttleInterestList :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> (Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleInterestListResp)))))
getShuttleInterestList a6 a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a4 a6 a5
        Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestList a6 a5 a3 a2 a1
    )

getShuttleInterestMapPoints :: Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> (Kernel.Types.Beckn.Context.City -> (DashboardUser -> (Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Environment.FlowHandler API.Types.Dashboard.AppManagement.ShuttleInterest.ShuttleInterestMapResp)))
getShuttleInterestMapPoints a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.enforceMerchantCityAccessForUser a2 a4 a3
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.ShuttleInterest.getShuttleInterestMapPoints a4 a3 a1
    )
