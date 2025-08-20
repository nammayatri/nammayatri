{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.BBPS
  ( API,
    handler,
  )
where

import qualified API.Types.UI.BBPS
import qualified Control.Lens
import qualified Domain.Action.UI.BBPS
import qualified Domain.Types.BBPS
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "bbps" :> "session" :> ReqBody '[JSON] API.Types.UI.BBPS.BBPSSessionReq
      :> Post
           '[JSON]
           Domain.Types.BBPS.BBPSSessionPayload
      :<|> TokenAuth
      :> "bbps"
      :> "createOrder"
      :> ReqBody '[JSON] API.Types.UI.BBPS.BBPSPaymentReq
      :> Post
           '[JSON]
           Kernel.External.Payment.Interface.CreateOrderResp
      :<|> TokenAuth
      :> "bbps"
      :> "getOrderStatus"
      :> Capture
           "orderId"
           Kernel.Prelude.Text
      :> Get
           '[JSON]
           API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
      :<|> "bbps"
      :> "confirm-payment"
      :> ReqBody
           '[JSON]
           API.Types.UI.BBPS.BBPSServerReq
      :> Post
           '[JSON]
           API.Types.UI.BBPS.BBPSServerResp
      :<|> "bbps"
      :> "payment-status"
      :> ReqBody
           '[JSON]
           API.Types.UI.BBPS.BBPSServerReq
      :> Post
           '[JSON]
           API.Types.UI.BBPS.BBPSServerResp
      :<|> TokenAuth
      :> "bbps"
      :> "orders"
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "active"
           Kernel.Prelude.Bool
      :> QueryParam
           "status"
           Domain.Types.BBPS.BBPSPaymentStatus
      :> Get
           '[JSON]
           [API.Types.UI.BBPS.BBPSInfoAPIRes]
  )

handler :: Environment.FlowServer API
handler = postBbpsSession :<|> postBbpsCreateOrder :<|> getBbpsGetOrderStatus :<|> postBbpsCrossCheckPayment :<|> postBbpsPaymentStatus :<|> getBbpsOrders

postBbpsSession ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSSessionReq ->
    Environment.FlowHandler Domain.Types.BBPS.BBPSSessionPayload
  )
postBbpsSession a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsSession (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postBbpsCreateOrder ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.BBPS.BBPSPaymentReq ->
    Environment.FlowHandler Kernel.External.Payment.Interface.CreateOrderResp
  )
postBbpsCreateOrder a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsCreateOrder (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getBbpsGetOrderStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.FlowHandler API.Types.UI.BBPS.BBPSPaymentStatusAPIRes
  )
getBbpsGetOrderStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.getBbpsGetOrderStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postBbpsCrossCheckPayment :: (API.Types.UI.BBPS.BBPSServerReq -> Environment.FlowHandler API.Types.UI.BBPS.BBPSServerResp)
postBbpsCrossCheckPayment a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsCrossCheckPayment a1

postBbpsPaymentStatus :: (API.Types.UI.BBPS.BBPSServerReq -> Environment.FlowHandler API.Types.UI.BBPS.BBPSServerResp)
postBbpsPaymentStatus a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.postBbpsPaymentStatus a1

getBbpsOrders ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Bool ->
    Kernel.Prelude.Maybe Domain.Types.BBPS.BBPSPaymentStatus ->
    Environment.FlowHandler [API.Types.UI.BBPS.BBPSInfoAPIRes]
  )
getBbpsOrders a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.BBPS.getBbpsOrders (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a5) a4 a3 a2 a1
