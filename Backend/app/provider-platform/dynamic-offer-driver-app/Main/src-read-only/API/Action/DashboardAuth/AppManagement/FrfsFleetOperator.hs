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
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import qualified Tools.ActorInfo
import Tools.Auth
import Tools.Auth.DashboardUserAuth

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction :<|> PostFrfsFleetOperatorV2CurrentOperation :<|> PostFrfsFleetOperatorV2TripAction))

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

type PostFrfsFleetOperatorV2CurrentOperation =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_CURRENT_OPERATION"
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorV2CurrentOperation
  )

type PostFrfsFleetOperatorV2TripAction =
  ( DashboardUserAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_TRIP_ACTION"
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorV2TripAction
  )

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city :<|> postFrfsFleetOperatorV2CurrentOperation merchantId city :<|> postFrfsFleetOperatorV2TripAction merchantId city

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation a4 a3 a1
    )

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_TRIP_ACTION" a2 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a2 $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction a4 a3 a1
    )

postFrfsFleetOperatorV2CurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2CurrentOperation a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_CURRENT_OPERATION" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2CurrentOperation a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )

postFrfsFleetOperatorV2TripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2TripAction a5 a4 a3 a2 a1 =
  withDashboardFlowHandlerAPI $
    ( do
        Tools.Auth.DashboardUserAuth.auditDashboardAction Tools.Auth.DashboardUserAuth.DRIVER_OFFER_BPP_MANAGEMENT "PROVIDER_APP_MANAGEMENT/FRFS_FLEET_OPERATOR/POST_FRFS_FLEET_OPERATOR_V2_TRIP_ACTION" a3 (Kernel.Prelude.Nothing :: Kernel.Prelude.Maybe ())
        Tools.ActorInfo.withDashboardUserActorInfo a3 $ Domain.Action.Dashboard.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2TripAction a5 a4 a2 (Kernel.Prelude.Just (Tools.Auth.DashboardUserAuth.dashboardRequestorId a3)) a1
    )
