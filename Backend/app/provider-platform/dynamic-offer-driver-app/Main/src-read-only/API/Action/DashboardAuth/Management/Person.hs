{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.Management.Person
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Management.Person
import qualified Control.Lens
import qualified Domain.Action.Dashboard.Management.Person
import qualified Domain.Action.DashboardAuth.Management.Person
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
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

type API = ("person" :> GetPersonList)

type GetPersonList = (DashboardUserAuth ('DRIVER_OFFER_BPP_MANAGEMENT) "PROVIDER_MANAGEMENT/PERSON/GET_PERSON_LIST" :> API.Types.ProviderPlatform.Management.Person.GetPersonList)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getPersonList merchantId city

getPersonList :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.ProviderPlatform.Management.Person.ListPersonRes)
getPersonList a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardUserActorInfo a4 $ Domain.Action.DashboardAuth.Management.Person.getPersonList a6 a5 a4 a3 a2 a1
