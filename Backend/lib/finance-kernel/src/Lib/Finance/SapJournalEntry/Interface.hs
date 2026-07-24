{-
  Finance.SapJournalEntry.Interface

  Input types for SAP journal entry operations.
  The actual operations are in Lib.Finance.SapJournalEntry.Service
-}
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}

module Lib.Finance.SapJournalEntry.Interface
  ( SapJournalEntryInput (..),
  )
where

import Kernel.Prelude
import Kernel.Types.Common (Currency, HighPrecMoney)
import Lib.Finance.Domain.Types.SapJournalEntry (JournalEntryStatus, TransactionType)

data SapJournalEntryInput = SapJournalEntryInput
  { belnr :: Maybe Text,
    batchId :: Text,
    blart :: Text,
    transactionType :: TransactionType,
    description :: Text,
    budat :: Text,
    bldat :: Text,
    gjahr :: Maybe Text,
    totalDebitAmount :: HighPrecMoney,
    totalCreditAmount :: HighPrecMoney,
    currency :: Currency,
    transactionCount :: Int,
    glNumber :: Maybe [Text],
    glName :: Maybe [Text],
    sapMessage :: Maybe Text,
    status :: JournalEntryStatus,
    periodStartTime :: UTCTime,
    periodEndTime :: UTCTime,
    rawResponse :: Maybe Text,
    merchantId :: Text,
    merchantOperatingCityId :: Text
  }
  deriving (Eq, Show, Generic)
