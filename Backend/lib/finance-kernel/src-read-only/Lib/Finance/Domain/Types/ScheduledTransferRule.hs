{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.ScheduledTransferRule where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.Account
import qualified Tools.Beam.UtilsTH

data ScheduledTransferRule = ScheduledTransferRule
  { accountType :: Lib.Finance.Domain.Types.Account.AccountType,
    batchSize :: Kernel.Prelude.Int,
    bulkStatusCheckBatchLimit :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    bulkStatusCheckIntervalMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    counterpartyType :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType,
    createdAt :: Kernel.Prelude.UTCTime,
    dayOfMonth :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    dayOfWeek :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    defaultRailKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    frequency :: Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferFrequency,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferRule,
    intervalDays :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    intervalHours :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    isEnabled :: Kernel.Prelude.Bool,
    maxRetries :: Kernel.Prelude.Int,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    minimumAmount :: Kernel.Types.Common.HighPrecMoney,
    name :: Kernel.Prelude.Text,
    orderType :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    remark :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    rescheduleBufferMinutes :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
    subLedger :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    timeDiffFromUtc :: Kernel.Types.Common.Seconds,
    timeOfDay :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON)

data ScheduledTransferFrequency = DAILY | WEEKLY | MONTHLY | HOURLY | EVERY_N_DAYS deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList ''ScheduledTransferFrequency)

$(mkHttpInstancesForEnum ''ScheduledTransferFrequency)
