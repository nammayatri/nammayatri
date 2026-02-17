{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.DriverWallet where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.HideSecrets
import qualified Lib.Payment.Domain.Types.Common
import qualified Lib.Payment.Domain.Types.PayoutRequest
import Servant
import Tools.Auth

data PayoutHistoryItem = PayoutHistoryItem
  { amount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    entityName :: Kernel.Prelude.Maybe Lib.Payment.Domain.Types.Common.EntityName,
    payoutFee :: Kernel.Types.Common.HighPrecMoney,
    payoutMethod :: Kernel.Prelude.Text,
    payoutVpa :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    status :: Lib.Payment.Domain.Types.PayoutRequest.PayoutRequestStatus,
    timestamp :: Data.Time.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data PayoutHistoryResponse = PayoutHistoryResponse
  { items :: [PayoutHistoryItem],
    lastPayout :: Kernel.Prelude.Maybe PayoutHistoryItem,
    totalPaidOut :: Kernel.Types.Common.HighPrecMoney,
    totalPending :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data TopUpRequest = TopUpRequest {amount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

instance Kernel.Types.HideSecrets.HideSecrets TopUpRequest where
  hideSecrets = Kernel.Prelude.identity

data WalletItem = WalletItem {itemName :: Kernel.Prelude.Text, itemReference :: Kernel.Prelude.Text, itemValue :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalletItemGroup = WalletItemGroup {items :: [WalletItem], totalAmount :: Kernel.Types.Common.HighPrecMoney}
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

data WalletSummaryResponse = WalletSummaryResponse
  { additions :: WalletItemGroup,
    currentBalance :: Kernel.Types.Common.HighPrecMoney,
    deductions :: WalletItemGroup,
    nonRedeemableBalance :: Kernel.Types.Common.HighPrecMoney,
    redeemableBalance :: Kernel.Types.Common.HighPrecMoney
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
