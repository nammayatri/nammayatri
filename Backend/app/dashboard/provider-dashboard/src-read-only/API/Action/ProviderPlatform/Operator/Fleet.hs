{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.Fleet
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.Fleet
import qualified Domain.Action.ProviderPlatform.Operator.Fleet
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = (PostFleetOperatorFleetRegister :<|> PostFleetOperatorFleetLink :<|> PostFleetOperatorFleetUnlink)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postFleetOperatorFleetRegister merchantId city :<|> postFleetOperatorFleetLink merchantId city :<|> postFleetOperatorFleetUnlink merchantId city

type PostFleetOperatorFleetRegister =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_OPERATOR) / ('API.Types.ProviderPlatform.Operator.FLEET) / ('API.Types.ProviderPlatform.Operator.Fleet.POST_FLEET_OPERATOR_FLEET_REGISTER))
      :> API.Types.ProviderPlatform.Operator.Fleet.PostFleetOperatorFleetRegister
  )

type PostFleetOperatorFleetLink =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_OPERATOR) / ('API.Types.ProviderPlatform.Operator.FLEET) / ('API.Types.ProviderPlatform.Operator.Fleet.POST_FLEET_OPERATOR_FLEET_LINK))
      :> API.Types.ProviderPlatform.Operator.Fleet.PostFleetOperatorFleetLink
  )

type PostFleetOperatorFleetUnlink =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_OPERATOR) / ('API.Types.ProviderPlatform.Operator.FLEET) / ('API.Types.ProviderPlatform.Operator.Fleet.POST_FLEET_OPERATOR_FLEET_UNLINK))
      :> API.Types.ProviderPlatform.Operator.Fleet.PostFleetOperatorFleetUnlink
  )

postFleetOperatorFleetRegister :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.Fleet.FleetOwnerRegisterReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetRegister merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Fleet.postFleetOperatorFleetRegister merchantShortId opCity apiTokenInfo req

postFleetOperatorFleetLink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetLink merchantShortId opCity apiTokenInfo fleetOwnerId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Fleet.postFleetOperatorFleetLink merchantShortId opCity apiTokenInfo fleetOwnerId

postFleetOperatorFleetUnlink :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Text -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postFleetOperatorFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.Fleet.postFleetOperatorFleetUnlink merchantShortId opCity apiTokenInfo fleetOwnerId
