{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.UI.DriverWallet 
( API,
handler )
where
import Storage.Beam.SystemConfigs ()
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.UI.DriverWallet
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Control.Lens
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified API.Types.UI.DriverWallet
import qualified Data.Time
import qualified Kernel.Types.APISuccess
import qualified Domain.Action.UI.Plan
import qualified Lib.Payment.Domain.Types.PayoutRequest
import qualified Domain.Types.MerchantOperatingCity



type API = (TokenAuth :> "wallet" :> "balance" :> Get ('[JSON]) API.Types.UI.DriverWallet.WalletBalanceResponse :<|> TokenAuth :> "wallet" :> "transactions" :> QueryParam "fromDate"
                                                                                                                                                                           Data.Time.UTCTime :> QueryParam "toDate" Data.Time.UTCTime :> Get ('[JSON])
                                                                                                                                                                                                                                             API.Types.UI.DriverWallet.WalletSummaryResponse :<|> TokenAuth :> "wallet" :> "payout" :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                            Kernel.Types.APISuccess.APISuccess :<|> TokenAuth :> "wallet" :> "topup" :> ReqBody ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                API.Types.UI.DriverWallet.TopUpRequest :> Post ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                               Domain.Action.UI.Plan.PlanSubscribeRes :<|> TokenAuth :> "wallet" :> "payoutHistory" :> QueryParam "from"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Data.Time.UTCTime :> QueryParam "to"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  Data.Time.UTCTime :> QueryParam "status"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  [Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus] :> QueryParam "limit"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             Kernel.Prelude.Int :> QueryParam "offset"
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              Kernel.Prelude.Int :> Get ('[JSON])
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        API.Types.UI.DriverWallet.PayoutHistoryResponse)
handler :: Environment.FlowServer API
handler = getWalletBalance :<|> getWalletTransactions :<|> postWalletPayout :<|> postWalletTopup :<|> getWalletPayoutHistory
getWalletBalance :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletBalanceResponse)
getWalletBalance a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.getWalletBalance (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
getWalletTransactions :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                           Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                           Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Environment.FlowHandler API.Types.UI.DriverWallet.WalletSummaryResponse)
getWalletTransactions a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.getWalletTransactions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a3) a2 a1
postWalletPayout :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Environment.FlowHandler Kernel.Types.APISuccess.APISuccess)
postWalletPayout a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.postWalletPayout (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
postWalletTopup :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                     Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                     Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> API.Types.UI.DriverWallet.TopUpRequest -> Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes)
postWalletTopup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.postWalletTopup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
getWalletPayoutHistory :: ((Kernel.Types.Id.Id Domain.Types.Person.Person,
                            Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
                            Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Kernel.Prelude.Maybe ([Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus]) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Environment.FlowHandler API.Types.UI.DriverWallet.PayoutHistoryResponse)
getWalletPayoutHistory a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.getWalletPayoutHistory (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1



