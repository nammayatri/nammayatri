{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.FrfsFleetOperator
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.FrfsFleetOperator
import qualified "dynamic-offer-driver-app" API.Types.UI.FRFSFleetOperator
import qualified Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator
import "dynamic-offer-driver-app" Domain.Types.AccessMatrix
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction :<|> PostFrfsFleetOperatorV2CurrentOperation :<|> PostFrfsFleetOperatorV2TripAction))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city :<|> postFrfsFleetOperatorV2CurrentOperation merchantId city :<|> postFrfsFleetOperatorV2TripAction merchantId city

type PostFrfsFleetOperatorCurrentOperation =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorCurrentOperation
  )

type PostFrfsFleetOperatorTripAction =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_TRIP_ACTION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorTripAction
  )

type PostFrfsFleetOperatorV2CurrentOperation =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_V2_CURRENT_OPERATION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorV2CurrentOperation
  )

type PostFrfsFleetOperatorV2TripAction =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_APP_MANAGEMENT) / ('API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR) / ('API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_V2_TRIP_ACTION))
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorV2TripAction
  )

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req

postFrfsFleetOperatorV2CurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2CurrentOperation merchantShortId opCity apiTokenInfo operatorId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2CurrentOperation merchantShortId opCity apiTokenInfo operatorId req

postFrfsFleetOperatorV2TripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo UserActionType -> Kernel.Prelude.Maybe (Kernel.Prelude.Text) -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionV2Req -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationV2Resp)
postFrfsFleetOperatorV2TripAction merchantShortId opCity apiTokenInfo operatorId req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorV2TripAction merchantShortId opCity apiTokenInfo operatorId req
