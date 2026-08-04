{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.Operator.FrfsTripManagement
  ( API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator
import qualified API.Types.ProviderPlatform.Operator.FrfsTripManagement
import qualified Domain.Action.ProviderPlatform.Operator.FrfsTripManagement
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("operator" :> PostOperatorFrfsTripAction)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = postOperatorFrfsTripAction merchantId city

type PostOperatorFrfsTripAction =
  ( ApiAuth
      ('DRIVER_OFFER_BPP_MANAGEMENT)
      ('DSL)
      (('PROVIDER_OPERATOR) / ('API.Types.ProviderPlatform.Operator.FRFS_TRIP_MANAGEMENT) / ('API.Types.ProviderPlatform.Operator.FrfsTripManagement.POST_OPERATOR_FRFS_TRIP_ACTION))
      :> API.Types.ProviderPlatform.Operator.FrfsTripManagement.PostOperatorFrfsTripAction
  )

postOperatorFrfsTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionRes)
postOperatorFrfsTripAction merchantShortId opCity apiTokenInfo req = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.Operator.FrfsTripManagement.postOperatorFrfsTripAction merchantShortId opCity apiTokenInfo req
