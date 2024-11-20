{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Management.PartnerOrgStation
  ( API.Types.RiderPlatform.Management.PartnerOrgStation.API,
    handler,
  )
where

import qualified API.Types.RiderPlatform.Management.PartnerOrgStation
import qualified Domain.Action.Dashboard.PartnerOrgStation as Domain.Action.Dashboard.PartnerOrgStation
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.RiderPlatform.Management.PartnerOrgStation.API)
handler merchantId city = postPartnerOrgStationCreatePartnerOrgStation merchantId city :<|> postPartnerOrgStationUpdatePartnerOrgStation merchantId city

postPartnerOrgStationCreatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.PartnerOrgStation.CreatePartnerOrgStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationCreatePartnerOrgStation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.PartnerOrgStation.postPartnerOrgStationCreatePartnerOrgStation a3 a2 a1

postPartnerOrgStationUpdatePartnerOrgStation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.RiderPlatform.Management.PartnerOrgStation.UpdatePartnerOrgStationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPartnerOrgStationUpdatePartnerOrgStation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.PartnerOrgStation.postPartnerOrgStationUpdatePartnerOrgStation a3 a2 a1
