{-# OPTIONS_GHC -Wno-unused-imports #-}

module API.Types.UI.SubscriptionTransaction where

import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Domain.Types.Location
import qualified Domain.Types.SubscriptionTransaction
import EulerHS.Prelude hiding (id)
import qualified Kernel.External.Payment.Juspay.Types.Common
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Servant
import Tools.Auth

data SubscriptionTransactionEntity = SubscriptionTransactionEntity
  { amount :: Kernel.Types.Common.HighPrecMoney,
    createdAt :: Data.Time.UTCTime,
    driverId :: Kernel.Prelude.Text,
    entityId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    fromLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    runningBalance :: Kernel.Types.Common.HighPrecMoney,
    status :: Kernel.External.Payment.Juspay.Types.Common.TransactionStatus,
    toLocation :: Kernel.Prelude.Maybe Domain.Types.Location.Location,
    transactionType :: Domain.Types.SubscriptionTransaction.TransactionType,
    updatedAt :: Data.Time.UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)
