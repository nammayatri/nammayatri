{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.Dashboard.AppManagement.SubscriptionTransaction (getSubscriptionTransactionSubscriptionTransactions) where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.UI.SubscriptionTransaction
import qualified Data.Time
import Domain.Action.UI.SubscriptionTransaction as SubscriptionTransaction
import qualified Domain.Types.Merchant
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.LedgerEntry
import SharedLogic.Merchant (findMerchantByShortId)
import qualified Storage.CachedQueries.Merchant.MerchantOperatingCity as CQMOC

getSubscriptionTransactionSubscriptionTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Environment.Flow [API.Types.UI.SubscriptionTransaction.SubscriptionTransactionEntity])
getSubscriptionTransactionSubscriptionTransactions merchantShortId opCity driverId fromDate limit maxAmount minAmount offset status toDate = do
  m <- findMerchantByShortId merchantShortId
  mOCityId <- CQMOC.getMerchantOpCityId Nothing m (Just opCity)
  SubscriptionTransaction.getSubscriptionTransactions (Just (Kernel.Types.Id.cast driverId), m.id, mOCityId) fromDate limit maxAmount minAmount offset status toDate
