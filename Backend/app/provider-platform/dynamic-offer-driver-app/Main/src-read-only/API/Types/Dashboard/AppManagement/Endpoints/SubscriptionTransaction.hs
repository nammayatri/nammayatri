{-# LANGUAGE StandaloneKindSignatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.Dashboard.AppManagement.Endpoints.SubscriptionTransaction where

import qualified API.Types.ProviderPlatform.Fleet.Driver
import qualified API.Types.UI.SubscriptionTransaction
import qualified Data.Aeson
import Data.OpenApi (ToSchema)
import qualified Data.Singletons.TH
import qualified Data.Time
import EulerHS.Prelude hiding (id, state)
import qualified EulerHS.Types
import qualified Kernel.Prelude
import Kernel.Types.Common
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Servant
import Servant.Client

type API = ("subscriptionTransaction" :> GetSubscriptionTransactionSubscriptionTransactionsHelper)

type GetSubscriptionTransactionSubscriptionTransactions =
  ( "subscription" :> "transactions" :> QueryParam "fromDate" Data.Time.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam "maxAmount" Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "minAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Lib.Finance.Domain.Types.LedgerEntry.EntryStatus
      :> QueryParam
           "toDate"
           Data.Time.UTCTime
      :> Get
           '[JSON]
           API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse
  )

type GetSubscriptionTransactionSubscriptionTransactionsHelper =
  ( Capture
      "driverId"
      (Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver)
      :> "subscription"
      :> "transactions"
      :> QueryParam "fromDate" Data.Time.UTCTime
      :> QueryParam
           "limit"
           Kernel.Prelude.Int
      :> QueryParam
           "maxAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "minAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam
           "offset"
           Kernel.Prelude.Int
      :> QueryParam
           "status"
           Lib.Finance.Domain.Types.LedgerEntry.EntryStatus
      :> QueryParam
           "toDate"
           Data.Time.UTCTime
      :> Get
           '[JSON]
           API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse
  )

newtype SubscriptionTransactionAPIs = SubscriptionTransactionAPIs {getSubscriptionTransactionSubscriptionTransactions :: Kernel.Types.Id.Id API.Types.ProviderPlatform.Fleet.Driver.Driver -> Kernel.Prelude.Maybe Data.Time.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.EntryStatus -> Kernel.Prelude.Maybe Data.Time.UTCTime -> EulerHS.Types.EulerClient API.Types.UI.SubscriptionTransaction.SubscriptionTransactionResponse}

mkSubscriptionTransactionAPIs :: (Client EulerHS.Types.EulerClient API -> SubscriptionTransactionAPIs)
mkSubscriptionTransactionAPIs subscriptionTransactionClient = (SubscriptionTransactionAPIs {..})
  where
    getSubscriptionTransactionSubscriptionTransactions = subscriptionTransactionClient

data SubscriptionTransactionUserActionType
  = GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS
  deriving stock (Show, Read, Generic, Eq, Ord)
  deriving anyclass (ToSchema)

instance ToJSON SubscriptionTransactionUserActionType where
  toJSON GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS = Data.Aeson.String "GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS"

instance FromJSON SubscriptionTransactionUserActionType where
  parseJSON (Data.Aeson.String "GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS") = pure GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS
  parseJSON _ = fail "GET_SUBSCRIPTION_TRANSACTION_SUBSCRIPTION_TRANSACTIONS expected"

$(Data.Singletons.TH.genSingletons [''SubscriptionTransactionUserActionType])
