{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Storage.Beam.ScheduledTransferRule where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Finance.Domain.Types.Account
import qualified Lib.Finance.Domain.Types.ScheduledTransferRule
import qualified Lib.Finance.Domain.Types.Transfer
import Tools.Beam.UtilsTH

data ScheduledTransferRuleT f = ScheduledTransferRuleT
  { accountType :: B.C f Lib.Finance.Domain.Types.Account.AccountType,
    batchSize :: B.C f Kernel.Prelude.Int,
    bulkStatusCheckBatchLimit :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    bulkStatusCheckIntervalMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    counterpartyType :: B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Account.CounterpartyType),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    dayOfMonth :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    dayOfWeek :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    defaultRailKey :: B.C f (Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Transfer.RailKey),
    frequency :: B.C f Lib.Finance.Domain.Types.ScheduledTransferRule.ScheduledTransferFrequency,
    id :: B.C f Kernel.Prelude.Text,
    intervalDays :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    intervalHours :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    isEnabled :: B.C f Kernel.Prelude.Bool,
    maxRetries :: B.C f Kernel.Prelude.Int,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    minimumAmount :: B.C f Kernel.Types.Common.HighPrecMoney,
    name :: B.C f Kernel.Prelude.Text,
    orderType :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    remark :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rescheduleBufferMinutes :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    subLedger :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    timeDiffFromUtc :: B.C f Kernel.Types.Common.Seconds,
    timeOfDay :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ScheduledTransferRuleT where
  data PrimaryKey ScheduledTransferRuleT f = ScheduledTransferRuleId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ScheduledTransferRuleId . id

type ScheduledTransferRule = ScheduledTransferRuleT Identity

$(enableKVPG ''ScheduledTransferRuleT ['id] [['name]])

$(mkTableInstancesGenericSchema ''ScheduledTransferRuleT "finance_scheduled_transfer_rule")
