{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.AppManagement.FrfsFleetOperator
  ( API.Types.Dashboard.AppManagement.FrfsFleetOperator.API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.FrfsFleetOperator
import qualified "this" API.Types.UI.FRFSFleetOperator
import qualified Domain.Action.Dashboard.AppManagement.FrfsFleetOperator
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.FrfsFleetOperator.API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation a3 a2 a1

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction a3 a2 a1
