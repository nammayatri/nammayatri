{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.RiderPlatform.AppManagement.FrfsFleetOperator
  ( API,
    handler,
  )
where

import qualified API.Types.Dashboard.AppManagement
import qualified "rider-app" API.Types.Dashboard.AppManagement.FrfsFleetOperator
import qualified "rider-app" API.Types.UI.FRFSTicketService
import qualified Domain.Action.RiderPlatform.AppManagement.FrfsFleetOperator
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city

type PostFrfsFleetOperatorCurrentOperation =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorCurrentOperation
  )

type PostFrfsFleetOperatorTripAction =
  ( ApiAuth
      ('APP_BACKEND_MANAGEMENT)
      ('DSL)
      (('RIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_TRIP_ACTION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorTripAction
  )

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.FRFSTicketService.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSTicketService.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.RiderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req
