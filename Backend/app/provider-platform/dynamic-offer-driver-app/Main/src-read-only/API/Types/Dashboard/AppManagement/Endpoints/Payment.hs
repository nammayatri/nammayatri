{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.Payment where

import qualified Dashboard.Common
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified "this" Domain.Action.UI.Payment
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Payment.Domain.Types.PaymentOrder
import Servant
import Servant.Client

type API = ("payment" :> (CreatePaymentOrderHelper :<|> GetPaymentOrderHelper :<|> GetPaymentOrderStatusHelper))

type CreatePaymentOrder = ("order" :> "create" :> Capture "invoiceId" Kernel.Prelude.Text :> Post '[JSON] Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp)

type CreatePaymentOrderHelper =
  ( "order" :> Capture "invoiceId" Kernel.Prelude.Text :> Capture "requestorId" (Kernel.Types.Id.Id Dashboard.Common.Person)
      :> Post
           '[JSON]
           Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
  )

type GetPaymentOrder = ("order" :> Capture "orderId" Kernel.Prelude.Text :> Get '[JSON] Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity)

type GetPaymentOrderHelper =
  ( "order" :> Capture "orderId" Kernel.Prelude.Text :> Capture "requestorId" (Kernel.Types.Id.Id Dashboard.Common.Person)
      :> Get
           '[JSON]
           Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity
  )

type GetPaymentOrderStatus = ("order" :> "status" :> Capture "orderId" Kernel.Prelude.Text :> Get '[JSON] Domain.Action.UI.Payment.PaymentStatusResp)

type GetPaymentOrderStatusHelper =
  ( "order" :> "status" :> Capture "orderId" Kernel.Prelude.Text :> Capture "requestorId" (Kernel.Types.Id.Id Dashboard.Common.Person)
      :> Get
           '[JSON]
           Domain.Action.UI.Payment.PaymentStatusResp
  )

data PaymentAPIs = PaymentAPIs
  { createPaymentOrder :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp,
    getPaymentOrder :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient Lib.Payment.Domain.Types.PaymentOrder.PaymentOrderAPIEntity,
    getPaymentOrderStatus :: Kernel.Prelude.Text -> Kernel.Types.Id.Id Dashboard.Common.Person -> EulerHS.Types.EulerClient Domain.Action.UI.Payment.PaymentStatusResp
  }

mkPaymentAPIs :: (Client EulerHS.Types.EulerClient API -> PaymentAPIs)
mkPaymentAPIs paymentClient = (PaymentAPIs {..})
  where
    createPaymentOrder :<|> getPaymentOrder :<|> getPaymentOrderStatus = paymentClient

data PaymentUserActionType
  = CREATE_PAYMENT_ORDER
  | GET_PAYMENT_ORDER
  | GET_PAYMENT_ORDER_STATUS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

$(Data.Singletons.TH.genSingletons [''PaymentUserActionType])
