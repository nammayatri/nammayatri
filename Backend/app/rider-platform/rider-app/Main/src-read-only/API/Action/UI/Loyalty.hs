{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.Loyalty
  ( API,
    handler,
  )
where

import qualified API.Types.UI.Loyalty
import qualified Control.Lens
import qualified Domain.Action.UI.Loyalty
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "wallet" :> "loyaltyInfo" :> Post ('[JSON]) API.Types.UI.Loyalty.LoyaltyInfoResp :<|> TokenAuth :> "rider" :> "monthlyExpense"
      :> Post
           ('[JSON])
           API.Types.UI.Loyalty.MonthlyExpenseResp
  )

handler :: Environment.FlowServer API
handler = postWalletLoyaltyInfo :<|> postRiderMonthlyExpense

postWalletLoyaltyInfo :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.Loyalty.LoyaltyInfoResp)
postWalletLoyaltyInfo a1 = withFlowHandlerAPI $ Domain.Action.UI.Loyalty.postWalletLoyaltyInfo (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)

postRiderMonthlyExpense :: ((Kernel.Types.Id.Id Domain.Types.Person.Person, Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Environment.FlowHandler API.Types.UI.Loyalty.MonthlyExpenseResp)
postRiderMonthlyExpense a1 = withFlowHandlerAPI $ Domain.Action.UI.Loyalty.postRiderMonthlyExpense (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a1)
