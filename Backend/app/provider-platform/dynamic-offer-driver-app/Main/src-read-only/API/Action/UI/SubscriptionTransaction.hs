{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Action.UI.SubscriptionTransaction
  ( API,
    handler,
  )
where

import qualified API.Types.UI.SubscriptionTransaction
import qualified Control.Lens
import qualified Data.Time
import qualified Domain.Action.UI.SubscriptionTransaction
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Environment
import EulerHS.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Finance.Domain.Types.LedgerEntry
import Servant
import Storage.Beam.SystemConfigs ()
import Tools.Auth

type API =
  ( TokenAuth :> "subscription" :> "transactions" :> QueryParam "fromDate" Data.Time.UTCTime :> QueryParam "limit" Kernel.Prelude.Int
      :> QueryParam
           "maxAmount"
           Kernel.Types.Common.HighPrecMoney
      :> QueryParam "minAmount" Kernel.Types.Common.HighPrecMoney
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
           [API.Types.UI.SubscriptionTransaction.SubscriptionTransactionEntity]
  )

handler :: Environment.FlowServer API
handler = getSubscriptionTransactions

getSubscriptionTransactions ::
  ( ( Kernel.Types.Id.Id Domain.Types.Person.Person,
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant,
      Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity
    ) ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
    Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney ->
    Kernel.Prelude.Maybe Kernel.Prelude.Int ->
    Kernel.Prelude.Maybe Lib.Finance.Domain.Types.LedgerEntry.EntryStatus ->
    Kernel.Prelude.Maybe Data.Time.UTCTime ->
    Environment.FlowHandler [API.Types.UI.SubscriptionTransaction.SubscriptionTransactionEntity]
  )
getSubscriptionTransactions a8 a7 a6 a5 a4 a3 a2 a1 = withFlowHandlerAPI $ Domain.Action.UI.SubscriptionTransaction.getSubscriptionTransactions (Control.Lens.over Control.Lens._1 Kernel.Prelude.Just a8) a7 a6 a5 a4 a3 a2 a1
