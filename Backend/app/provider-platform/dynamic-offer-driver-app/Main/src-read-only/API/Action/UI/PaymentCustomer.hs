{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.PaymentCustomer
  ( API,
    handler,
  )
where

import qualified API.Types.UI.PaymentCustomer
import qualified Control.Lens
import qualified Domain.Action.UI.PaymentCustomer
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.Plan
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import qualified Tools.ActorInfo
import Tools.Auth

type API = (TokenAuth :> "payment" :> "customer" :> QueryParam "serviceName" Domain.Types.Plan.ServiceNames :> Get ('[JSON]) API.Types.UI.PaymentCustomer.PaymentCustomerResp)

handler :: Environment.FlowServer API
handler = getPaymentCustomer

getPaymentCustomer ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe (Domain.Types.Plan.ServiceNames) ->
    Environment.FlowHandler API.Types.UI.PaymentCustomer.PaymentCustomerResp
  )
getPaymentCustomer a2 a1 = withFlowHandlerAPI $ Tools.ActorInfo.withPersonIdActorInfo (Control.Lens.view Control.Lens._1 a2) $ Domain.Action.UI.PaymentCustomer.getPaymentCustomer (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
