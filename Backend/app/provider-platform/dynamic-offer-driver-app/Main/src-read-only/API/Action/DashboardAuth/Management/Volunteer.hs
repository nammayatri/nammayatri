{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Volunteer
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Volunteer
import qualified Domain.Action.Dashboard.Management.Volunteer
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

type API = ("volunteer" :> (PostVolunteerCreate :<|> GetVolunteerList :<|> PostVolunteerUpdate))

type PostVolunteerCreate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/VOLUNTEER/POST_VOLUNTEER_CREATE"
      :> API.Types.ProviderPlatform.Management.Volunteer.PostVolunteerCreate
  )

type GetVolunteerList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/VOLUNTEER/GET_VOLUNTEER_LIST" :> API.Types.ProviderPlatform.Management.Volunteer.GetVolunteerList)

type PostVolunteerUpdate =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_MANAGEMENT/VOLUNTEER/POST_VOLUNTEER_UPDATE"
      :> API.Types.ProviderPlatform.Management.Volunteer.PostVolunteerUpdate
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postVolunteerCreate merchantId city :<|> getVolunteerList merchantId city :<|> postVolunteerUpdate merchantId city

postVolunteerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerRes)
postVolunteerCreate a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.postVolunteerCreate a4 a3 a1

getVolunteerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Bool) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.VolunteerListRes)
getVolunteerList a8 a7 _a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.getVolunteerList a8 a7 a5 a4 a3 a2 a1

postVolunteerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Volunteer.UpdateVolunteerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerUpdate a6 a5 _a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.postVolunteerUpdate a6 a5 a3 a2 a1
