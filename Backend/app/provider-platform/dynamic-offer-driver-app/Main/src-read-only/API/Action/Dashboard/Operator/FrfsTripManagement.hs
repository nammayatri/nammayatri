{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.Dashboard.Operator.FrfsTripManagement
  ( API.Types.ProviderPlatform.Operator.FrfsTripManagement.API,
    handler,
  )
where

import qualified API.Types.ProviderPlatform.Operator.FrfsTripManagement
import qualified Domain.Action.Dashboard.Operator.FrfsTripManagement
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.ProviderPlatform.Operator.FrfsTripManagement.API)
handler merchantId city = postOperatorFrfsTripAction merchantId city

postOperatorFrfsTripAction :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionReq -> Environment.FlowHandler API.Types.ProviderPlatform.Operator.FrfsTripManagement.OperatorTripActionRes)
postOperatorFrfsTripAction a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.Operator.FrfsTripManagement.postOperatorFrfsTripAction a3 a2 a1
