{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.RidePayment
  ( API,
    handler,
  )
where

import qualified API.Types.UI.RidePayment
import qualified Control.Lens
import qualified Domain.Action.UI.RidePayment
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Ride
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "payment" :> "methods" :> Get '[JSON] API.Types.UI.RidePayment.PaymentMethodsResponse :<|> TokenAuth :> "payment" :> "methods"
      :> Capture
           "paymentMethodId"
           Kernel.External.Payment.Interface.Types.PaymentMethodId
      :> "makeDefault"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payment"
      :> "intent"
      :> "setup"
      :> Get
           '[JSON]
           API.Types.UI.RidePayment.SetupIntentResponse
      :<|> TokenAuth
      :> "payment"
      :> "intent"
      :> "payment"
      :> Get
           '[JSON]
           API.Types.UI.RidePayment.PaymentIntentResponse
      :<|> TokenAuth
      :> "payment"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "method"
      :> Capture
           "paymentMethodId"
           Kernel.External.Payment.Interface.Types.PaymentMethodId
      :> "update"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payment"
      :> "methods"
      :> Capture
           "paymentMethodId"
           Kernel.External.Payment.Interface.Types.PaymentMethodId
      :> "delete"
      :> Delete
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payment"
      :> Capture
           "rideId"
           (Kernel.Types.Id.Id Domain.Types.Ride.Ride)
      :> "addTip"
      :> ReqBody
           '[JSON]
           API.Types.UI.RidePayment.AddTipRequest
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payment"
      :> "customer"
      :> Get
           '[JSON]
           Kernel.External.Payment.Interface.Types.CreateCustomerResp
  )

handler :: Environment.FlowServer API
handler = getPaymentMethods :<|> postPaymentMethodsMakeDefault :<|> getPaymentIntentSetup :<|> getPaymentIntentPayment :<|> postPaymentMethodUpdate :<|> deletePaymentMethodsDelete :<|> postPaymentAddTip :<|> getPaymentCustomer

getPaymentMethods :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.RidePayment.PaymentMethodsResponse)
getPaymentMethods a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.getPaymentMethods (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPaymentMethodsMakeDefault ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.External.Payment.Interface.Types.PaymentMethodId ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPaymentMethodsMakeDefault a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.postPaymentMethodsMakeDefault (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPaymentIntentSetup :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.RidePayment.SetupIntentResponse)
getPaymentIntentSetup a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.getPaymentIntentSetup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getPaymentIntentPayment ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler API.Types.UI.RidePayment.PaymentIntentResponse
  )
getPaymentIntentPayment a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.getPaymentIntentPayment (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPaymentMethodUpdate ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    Kernel.External.Payment.Interface.Types.PaymentMethodId ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPaymentMethodUpdate a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.postPaymentMethodUpdate (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

deletePaymentMethodsDelete ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.External.Payment.Interface.Types.PaymentMethodId ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
deletePaymentMethodsDelete a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.deletePaymentMethodsDelete (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

postPaymentAddTip ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Ride.Ride ->
    API.Types.UI.RidePayment.AddTipRequest ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPaymentAddTip a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.postPaymentAddTip (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

getPaymentCustomer ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Environment.FlowHandler Kernel.External.Payment.Interface.Types.CreateCustomerResp
  )
getPaymentCustomer a1 = withFlowHandlerAPI $ Domain.Action.UI.RidePayment.getPaymentCustomer (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
