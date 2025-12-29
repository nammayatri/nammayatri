{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.Volunteer
  ( API.Types.ProviderPlatform.Management.Volunteer.API,
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

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Management.Volunteer.API)
handler merchantId city = postVolunteerCreate merchantId city :<|> getVolunteerList merchantId city :<|> postVolunteerUpdate merchantId city

postVolunteerCreate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerReq -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.CreateVolunteerRes)
postVolunteerCreate a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.postVolunteerCreate a3 a2 a1

getVolunteerList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Volunteer.VolunteerListRes)
getVolunteerList a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.getVolunteerList a8 a7 a6 a5 a4 a3 a2 a1

postVolunteerUpdate :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Text -> Kernel.Prelude.Text -> API.Types.ProviderPlatform.Management.Volunteer.UpdateVolunteerReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postVolunteerUpdate a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Management.Volunteer.postVolunteerUpdate a5 a4 a3 a2 a1
