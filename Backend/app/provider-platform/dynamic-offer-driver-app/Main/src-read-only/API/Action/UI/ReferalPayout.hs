{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.ReferalPayout where

import qualified API.Types.UI.ReferalPayout
import qualified Control.Lens
import qualified Data.Text
import qualified Data.Time.Calendar
import qualified Domain.Action.UI.ReferalPayout as Domain.Action.UI.ReferalPayout
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Types.CreateOrder
import qualified Kernel.External.Payout.Interface.Types
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "referral" :> "earnings" :> MandatoryQueryParam "fromDate" Data.Time.Calendar.Day :> MandatoryQueryParam "toDate" Data.Time.Calendar.Day
      :> Get
           '[JSON]
           API.Types.UI.ReferalPayout.ReferralEarningsRes
      :<|> TokenAuth
      :> "delete"
      :> "vpa"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payout"
      :> "registration"
      :> Get
           '[JSON]
           Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
      :<|> TokenAuth
      :> "payout"
      :> "createOrder"
      :> ReqBody
           '[JSON]
           Kernel.External.Payout.Interface.Types.CreatePayoutOrderReq
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "payout"
      :> "order"
      :> "status"
      :> QueryParam
           "dailyStatsId"
           Data.Text.Text
      :> MandatoryQueryParam
           "orderId"
           Data.Text.Text
      :> Get
           '[JSON]
           Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp
  )

handler :: Environment.FlowServer API
handler = getReferralEarnings :<|> postDeleteVpa :<|> getPayoutRegistration :<|> postPayoutCreateOrder :<|> getPayoutOrderStatus

getReferralEarnings ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Data.Time.Calendar.Day ->
    Data.Time.Calendar.Day ->
    Environment.FlowHandler API.Types.UI.ReferalPayout.ReferralEarningsRes
  )
getReferralEarnings a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.getReferralEarnings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1

postDeleteVpa ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postDeleteVpa a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.postDeleteVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

getPayoutRegistration ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.External.Payment.Juspay.Types.CreateOrder.CreateOrderResp
  )
getPayoutRegistration a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.getPayoutRegistration (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postPayoutCreateOrder ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.External.Payout.Interface.Types.CreatePayoutOrderReq ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postPayoutCreateOrder a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.postPayoutCreateOrder (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1

getPayoutOrderStatus ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Text.Text ->
    Data.Text.Text ->
    Environment.FlowHandler Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp
  )
getPayoutOrderStatus a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferalPayout.getPayoutOrderStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
