{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.ReferralPayout 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.ReferralPayout
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Data.Time.Calendar
import qualified API.Types.UI.ReferralPayout
import qualified Kernel.Types.APISuccess
import qualified Domain.Action.UI.Driver
import qualified Kernel.External.Payout.Interface.Types
import qualified Data.Text
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "payout" :> "referral" :> "earnings" :> MandatoryQueryParam "fromDate" Data.Time.Calendar.Day :> MandatoryQueryParam "toDate" Data.Time.Calendar.Day :> Get ('[JSON])
                                                                                                                                                                                     API.Types.UI.ReferralPayout.ReferralEarningsRes :<|> TokenAuth :> "payout" :> "delete" :> "vpa" :> Post ('[JSON])
                                                                                                                                                                                                                                                                                             Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "payout" :> "update" :> "vpa" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                           API.Types.UI.ReferralPayout.UpdatePayoutVpaReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                  Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "payout" :> "registration" :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Domain.Action.UI.Driver.ClearDuesRes :<|> TokenAuth :> "payout" :> "createOrder" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Kernel.External.Payout.Interface.Types.CreatePayoutOrderReq :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "payout" :> "order" :> "status" :> MandatoryQueryParam "orderId"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Data.Text.Text :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
handler :: Environment.FlowServer API
handler = getPayoutReferralEarnings :<|> postPayoutDeleteVpa :<|> postPayoutUpdateVpa :<|> getPayoutRegistration :<|> postPayoutCreateOrder :<|> getPayoutOrderStatus
getPayoutReferralEarnings :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                               Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                               Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Data.Time.Calendar.Day -> Data.Time.Calendar.Day -> Environment.FlowHandler API.Types.UI.ReferralPayout.ReferralEarningsRes)
getPayoutReferralEarnings a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.getPayoutReferralEarnings (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
postPayoutDeleteVpa :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                         Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutDeleteVpa a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.postPayoutDeleteVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
postPayoutUpdateVpa :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                         Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                         Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.ReferralPayout.UpdatePayoutVpaReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutUpdateVpa a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.postPayoutUpdateVpa (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getPayoutRegistration :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler Domain.Action.UI.Driver.ClearDuesRes)
getPayoutRegistration a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.getPayoutRegistration (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
postPayoutCreateOrder :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.External.Payout.Interface.Types.CreatePayoutOrderReq -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postPayoutCreateOrder a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.postPayoutCreateOrder (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getPayoutOrderStatus :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                          Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                          Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Data.Text.Text -> Environment.FlowHandler Kernel.External.Payout.Interface.Types.PayoutOrderStatusResp)
getPayoutOrderStatus a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.ReferralPayout.getPayoutOrderStatus (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1



