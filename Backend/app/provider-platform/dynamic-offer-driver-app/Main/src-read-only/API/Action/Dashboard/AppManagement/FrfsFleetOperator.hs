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
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.FrfsFleetOperator.API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city :<|> postFrfsFleetOperatorV2CurrentOperation merchantId city :<|> postFrfsFleetOperatorV2TripAction merchantId city

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation a3 a2 a1

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction a3 a2 a1

postFrfsFleetOperatorV2CurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2CurrentOperation a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2CurrentOperation a5 a4 a3 a2 a1

postFrfsFleetOperatorV2TripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2TripAction a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Tools.ActorInfo.withDashboardMbPersonIdActorInfo (Kernel.Types.Id.Id <$> a2) $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2TripAction a5 a4 a3 a2 a1
