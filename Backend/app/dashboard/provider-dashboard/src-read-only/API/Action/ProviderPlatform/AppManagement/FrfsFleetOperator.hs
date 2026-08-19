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
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("FrfsFleetOperator" :> (PostFrfsFleetOperatorCurrentOperation :<|> PostFrfsFleetOperatorTripAction))

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFrfsFleetOperatorCurrentOperation merchantId city :<|> postFrfsFleetOperatorTripAction merchantId city

type PostFrfsFleetOperatorCurrentOperation =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR / 'API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_CURRENT_OPERATION)
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorCurrentOperation
  )

type PostFrfsFleetOperatorTripAction =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.FRFS_FLEET_OPERATOR / 'API.Types.Dashboard.AppManagement.FrfsFleetOperator.POST_FRFS_FLEET_OPERATOR_TRIP_ACTION)
      :> API.Types.Dashboard.AppManagement.FrfsFleetOperator.PostFrfsFleetOperatorTripAction
  )

postFrfsFleetOperatorCurrentOperation :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorCurrentOperationResp)
postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorCurrentOperation merchantShortId opCity apiTokenInfo req

postFrfsFleetOperatorTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionReq -> Environment.FlowHandler API.Types.UI.FRFSFleetOperator.FleetOperatorTripActionResp)
postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.FrfsFleetOperator.postFrfsFleetOperatorTripAction merchantShortId opCity apiTokenInfo req
