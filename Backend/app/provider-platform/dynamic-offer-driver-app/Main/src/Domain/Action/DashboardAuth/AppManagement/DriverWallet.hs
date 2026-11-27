{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Hand-written handlers for the direct-dashboard wallet screens that show item names: they are
-- shown in the dashboard user's language, English when the user has none set.
module Domain.Action.DashboardAuth.AppManagement.DriverWallet
  ( getDriverWalletWalletTransactions,
    getDriverWalletWalletTransactionHistory,
  )
where

import qualified API.Types.UI.DriverWallet
import qualified Domain.Action.Dashboard.AppManagement.DriverWallet
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Person.Type as Fields
import qualified Environment
import EulerHS.Prelude
import Kernel.External.Types (Language (..))
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import Servant
import Tools.Auth
import qualified Tools.Auth.DashboardUser as Fields
import Tools.Auth.DashboardUserAuth

getDriverWalletWalletTransactions :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe API.Types.UI.DriverWallet.AggregationLevel -> Environment.Flow API.Types.UI.DriverWallet.WalletSummaryResponse)
getDriverWalletWalletTransactions a7 a6 a5 a4 a3 a2 a1 = Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletTransactions a7 a6 (fromMaybe ENGLISH a5.person.language) a4 a3 a2 a1

getDriverWalletWalletTransactionHistory :: (Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> DashboardUser -> Kernel.Types.Id.Id Domain.Types.Person.Driver -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Prelude.Maybe Kernel.Prelude.Int -> Environment.Flow API.Types.UI.DriverWallet.WalletTransactionHistoryResponse)
getDriverWalletWalletTransactionHistory a8 a7 a6 a5 a4 a3 a2 a1 = Domain.Action.Dashboard.AppManagement.DriverWallet.getDriverWalletWalletTransactionHistory a8 a7 (fromMaybe ENGLISH a6.person.language) a5 a4 a3 a2 a1
