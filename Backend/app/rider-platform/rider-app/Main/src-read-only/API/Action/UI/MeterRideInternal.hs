{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.MeterRideInternal
  ( API,
    handler,
  )
where

import qualified API.Types.UI.MeterRideInternal
import qualified Data.Text
import qualified Domain.Action.UI.MeterRideInternal
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( "getCustomerInfo" :> Header "token" Data.Text.Text :> ReqBody '[JSON] API.Types.UI.MeterRideInternal.CustomerInfo
      :> Get
           '[JSON]
           API.Types.UI.MeterRideInternal.CustomerInfoResponse
      :<|> Capture "bppRideId" Data.Text.Text
      :> "addDestination"
      :> Header "token" Data.Text.Text
      :> ReqBody
           '[JSON]
           API.Types.UI.MeterRideInternal.MeterRideAddDestinationReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
  )

handler :: Environment.FlowServer API
handler = getGetCustomerInfo :<|> postAddDestination

getGetCustomerInfo :: (Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.MeterRideInternal.CustomerInfo -> Environment.FlowHandler API.Types.UI.MeterRideInternal.CustomerInfoResponse)
getGetCustomerInfo a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MeterRideInternal.getGetCustomerInfo a2 a1

postAddDestination :: (Data.Text.Text -> Kernel.Prelude.Maybe Data.Text.Text -> API.Types.UI.MeterRideInternal.MeterRideAddDestinationReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postAddDestination a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.MeterRideInternal.postAddDestination a3 a2 a1
