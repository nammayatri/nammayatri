{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.DriverWallet
  ( API,
    handler,
  )
where

import qualified API.Types.UI.DriverWallet
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.UI.DriverWallet
import qualified Domain.Action.UI.Plan
import qualified Domain.Types.DriverWallet
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.APISuccess
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "wallet" :> "transactions" :> QueryParam "fromDate" Data.Time.UTCTime :> QueryParam "toDate" Data.Time.UTCTime
      :> QueryParam
           "transactionType"
           Domain.Types.DriverWallet.WalletTransactionType
      :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam "offset" Kernel.Prelude.Int
      :> Get
           '[JSON]
           API.Types.UI.DriverWallet.TransactionResponse
      :<|> TokenAuth
      :> "wallet"
      :> "payout"
      :> Post
           '[JSON]
           Kernel.Types.APISuccess.APISuccess
      :<|> TokenAuth
      :> "wallet"
      :> "topup"
      :> ReqBody
           '[JSON]
           API.Types.UI.DriverWallet.TopUpRequest
      :> Post
           '[JSON]
           Domain.Action.UI.Plan.PlanSubscribeRes
  )

handler :: Environment.FlowServer API
handler = getWalletTransactions :<|> postWalletPayout :<|> postWalletTopup

getWalletTransactions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Domain.Types.DriverWallet.WalletTransactionType ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Environment.FlowHandler API.Types.UI.DriverWallet.TransactionResponse
  )
getWalletTransactions a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.getWalletTransactions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a6) a5 a4 a3 a2 a1

postWalletPayout ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Environment.FlowHandler Kernel.Types.APISuccess.APISuccess
  )
postWalletPayout a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.postWalletPayout (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postWalletTopup ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    API.Types.UI.DriverWallet.TopUpRequest ->
    Environment.FlowHandler Domain.Action.UI.Plan.PlanSubscribeRes
  )
postWalletTopup a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.DriverWallet.postWalletTopup (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a2) a1
