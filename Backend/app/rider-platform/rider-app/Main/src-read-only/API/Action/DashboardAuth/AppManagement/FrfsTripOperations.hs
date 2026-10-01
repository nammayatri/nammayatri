{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.DashboardAuth.AppManagement.FrfsTripOperations
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement.FrfsTripOperations
import qualified "this" API.Types.UI.FRFSTicketService
import qualified Domain.Action.Dashboard.AppManagement.FrfsTripOperations
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
import Tools.Auth.DashboardUserAuth

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

type PostFrfsFleetOperatorCurrentOperation =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TRIP_OPERATIONS/POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION"
      :> API.Types.Dashboard.AppManagement.FrfsTripOperations.PostFrfsFleetOperatorCurrentOperation
  )

type PostFrfsFleetOperatorTripAction =
  ( DashboardUserAuth
      ('APP_BACKEND_MANAGEMENT)
      "RIDER_APP_MANAGEMENT/FRFS_TRIP_OPERATIONS/POST_FRFS_FLEET_OPERATOR_TRIP_ACTION"
      :> API.Types.Dashboard.AppManagement.FrfsTripOperations.PostFrfsFleetOperatorTripAction
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/FRFS_TRIP_OPERATIONS/POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.FrfsTripOperations.postFrfsFleetOperatorCurrentOperation a4 a3 a1
    )

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.APP_BACKEND_MANAGEMENT "RIDER_APP_MANAGEMENT/FRFS_TRIP_OPERATIONS/POST_FRFS_FLEET_OPERATOR_TRIP_ACTION" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.FrfsTripOperations.postFrfsFleetOperatorTripAction a4 a3 a1
    )
