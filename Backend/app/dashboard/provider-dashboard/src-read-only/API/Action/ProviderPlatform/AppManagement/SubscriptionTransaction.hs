{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.ProviderPlatform.AppManagement.SubscriptionTransaction
  ( API,
    handler,
  )
where

import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement
import qualified "dynamic-offer-driver-app" API.Types.Dashboard.AppManagement.SubscriptionTransaction
import qualified API.Types.UI.SubscriptionTransaction
import qualified Data.Time
import qualified Domain.Action.ProviderPlatform.AppManagement.SubscriptionTransaction
import qualified "lib-dashboard" Domain.Types.Merchant
import qualified "lib-dashboard" Environment
import EulerHS.Prelude hiding (sortOn)
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common hiding (INFO)
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Servant
import Storage.Beam.CommonInstances ()
import Tools.Auth.Api

type API = ("subscriptionTransaction" :> GetSubscriptionTransactionSubscriptionTransactions)

handler :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> Environment.FlowServer API)
handler merchantId city = getSubscriptionTransactionSubscriptionTransactions merchantId city

type GetSubscriptionTransactionSubscriptionTransactions =
  ( ApiAuth
      'DRIVER_OFFER_BPP_MANAGEMENT
      'DSL
      ('PROVIDER_APP_MANAGEMENT / 'API.Types.Dashboard.AppManagement.SUBSCRIPTION_TRANSACTION / 'API.Types.Dashboard.AppManagement.SubscriptionTransaction.GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS)
      :> API.Types.Dashboard.AppManagement.SubscriptionTransaction.GetSubscriptionTransactionSubscriptionTransactions
  )

getSubscriptionTransactionSubscriptionTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> ApiTokenInfo -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Environment.FlowHandler API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse)
getSubscriptionTransactionSubscriptionTransactions merchantShortId opCity apiTokenInfo fromDate limit maxAmount minAmount offset status toDate = withFlowHandlerAPI' $ Domain.Action.ProviderPlatform.AppManagement.SubscriptionTransaction.getSubscriptionTransactionSubscriptionTransactions merchantShortId opCity apiTokenInfo fromDate limit maxAmount minAmount offset status toDate
