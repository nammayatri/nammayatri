{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handler for the direct-dashboard subscription-transaction list.
--
-- Its Helper takes the driver as an @Id Driver@ capture, and provider-dashboard
-- filled it with the calling operator's person id (@cast apiTokenInfo.personId@).
-- The generator only derives plain-text caller ids, so this conversion lives here.
module Domain.Action.DashboardAuth.AppManagement.SubscriptionTransaction
  ( getSubscriptionTransactionSubscriptionTransactions,
  )
where

import qualified API.Types.Dashboard.AppManagement.SubscriptionTransaction
import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.UI.SubscriptionTransaction
import qualified Data.Time
import qualified Domain.Action.Dashboard.AppManagement.SubscriptionTransaction
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Servant
import Tools.Auth
import Tools.Auth.DashboardUserAuth

getSubscriptionTransactionSubscriptionTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Types.Common.HighPrecMoney) -> Kernel.Prelude.Maybe (Kernel.Prelude.Int) -> Kernel.Prelude.Maybe (Lib.Finance.Domain.Types.LedgerEntry.EntryStatus) -> Kernel.Prelude.Maybe (Data.Time.UTCTime) -> Environment.Flow API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse)
getSubscriptionTransactionSubscriptionTransactions a10 a9 a8 a7 a6 a5 a4 a3 a2 a1 = Domain.Action.Dashboard.AppManagement.SubscriptionTransaction.getSubscriptionTransactionSubscriptionTransactions a10 a9 (Kernel.Types.Id.Id (Tools.Auth.DashboardUserAuth.dashboardRequestorId a8)) a7 a6 a5 a4 a3 a2 a1
