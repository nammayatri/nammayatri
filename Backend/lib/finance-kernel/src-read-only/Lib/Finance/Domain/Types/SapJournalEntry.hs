{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.SapJournalEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SapJournalEntry = SapJournalEntry
  { batchId :: Kernel.Prelude.Text,
    belnr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    blart :: Kernel.Prelude.Text,
    bldat :: Kernel.Prelude.Text,
    budat :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    description :: Kernel.Prelude.Text,
    gjahr :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    glName :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    glNumber :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.SapJournalEntry.SapJournalEntry,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    rawResponse :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    sapMessage :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    totalCreditAmount :: Kernel.Types.Common.HighPrecMoney,
    totalDebitAmount :: Kernel.Types.Common.HighPrecMoney,
    transactionCount :: Kernel.Prelude.Int,
    transactionType :: Lib.Finance.Domain.Types.SapJournalEntry.TransactionType,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data TransactionType = SubscriptionPurchase | Order | Refund | Chargeback | RevenueRecognition deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransactionType))
