{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.FrfsFleetOperator
  ( API,
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
import Tools.Auth.DashboardUserAuth

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

type PostFrfsFleetOperatorCurrentOperation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION"
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorCurrentOperation
  )

type PostFrfsFleetOperatorTripAction =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_TRIP_ACTION"
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorTripAction
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation a4 a3 a1

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction a4 a3 _a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction a4 a3 a1
