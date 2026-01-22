module API where

import Data.Aeson (Value)
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Interface.Types as Payment
import qualified Lib.Payment.Domain.Action as PaymentAction
import Servant

type ExternalPaymentAPI =
  "payment"
    :> "external"
    :> Capture "merchantShortId" Text
    :> "service"
    :> "juspay"
    :> "payment"
    :> QueryParam "city" Text
    :> QueryParam "serviceType" Text
    :> ReqBody '[JSON] PaymentAction.PaymentStatusResp
    :> Post '[JSON] Value

type InternalOrderStatusAPI =
  "payment"
    :> "internal"
    :> "orders"
    :> Capture "orderId" Text
    :> "status"
    :> Get '[JSON] Payment.OrderStatusResp

type API = ExternalPaymentAPI :<|> InternalOrderStatusAPI

api :: Proxy API
api = Proxy
