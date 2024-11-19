{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.UpdatePartnerOrgStationID
  ( API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID
import qualified Domain.Action.Dashboard.UpdatePartnerOrgStationID as Domain.Action.Dashboard.UpdatePartnerOrgStationID
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.API)
handler merchantId city = postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID merchantId city

postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.UpdatePartnerOrgStationID.ReqData -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.UpdatePartnerOrgStationID.postUpdatePartnerOrgStationIDUpdatePartnerOrgStationID a3 a2 a1
