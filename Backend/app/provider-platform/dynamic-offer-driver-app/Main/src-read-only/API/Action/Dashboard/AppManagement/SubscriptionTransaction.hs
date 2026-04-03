{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module API.Action.Dashboard.AppManagement.SubscriptionTransaction 
( API.Types.Dashboard.AppManagement.SubscriptionTransaction.API,
handler )
where
import EulerHS.Prelude
import Servant
import Tools.Auth
import Kernel.Utils.Common
import qualified Domain.Action.Dashboard.AppManagement.SubscriptionTransaction
import qualified Kernel.Prelude
import qualified Domain.Types.Merchant
import qualified Environment
import qualified Kernel.Types.Id
import qualified Kernel.Types.Beckn.Context
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified Data.Time
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified API.Types.UI.SubscriptionTransaction
import qualified API.Types.Dashboard.AppManagement.SubscriptionTransaction



handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API.Types.Dashboard.AppManagement.SubscriptionTransaction.API)
handler merchantId city = getSubscriptionTransactionSubscriptionTransactions merchantId city
getSubscriptionTransactionSubscriptionTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Environment.FlowHandler API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse)
getSubscriptionTransactionSubscriptionTransactions a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = withDashboardFlowHandlerAPI $ Domain.Action.Dashboard.AppManagement.SubscriptionTransaction.getSubscriptionTransactionSubscriptionTransactions a10 a9 a8 a7 a6 a5 a4 a3 a2 a1



