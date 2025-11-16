{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.ProviderPlatform.AppManagement.SubscriptionTransaction (getSubscriptionTransactionSubscriptionTransactions) where

import qualified API.Client.ProviderPlatform.AppManagement
import qualified API.Types.UI.SubscriptionTransaction
import qualified Data.Time
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude
import qualified Kernel.External.Payment.Juspay.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api
import Tools.Auth.Merchant

getSubscriptionTransactionSubscriptionTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.External.Payment.Juspay.Types.Common.TransactionStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Environment.Flow [API.Types.UI.SubscriptionTransaction.SubscriptionTransactionEntity])
getSubscriptionTransactionSubscriptionTransactions merchantShortId opCity apiTokenInfo fromDate limit maxAmount minAmount offset status toDate = do
  checkedMerchantId <- merchantCityAccessCheck merchantShortId apiTokenInfo.merchant.shortId opCity apiTokenInfo.city
  API.Client.ProviderPlatform.AppManagement.callAppManagementAPI checkedMerchantId opCity (.subscriptionTransactionDSL.getSubscriptionTransactionSubscriptionTransactions) (Kernel.Types.Id.cast apiTokenInfo.personId) fromDate limit maxAmount minAmount offset status toDate
