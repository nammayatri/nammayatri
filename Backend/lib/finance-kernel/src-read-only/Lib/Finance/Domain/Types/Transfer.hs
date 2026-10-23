{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.Transfer (module Lib.Finance.Domain.Types.Transfer, module ReExport) where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Domain.Types.Account
import Lib.Finance.Domain.Types.Extra.Transfer as ReExport
import qualified Lib.Finance.Domain.Types.Extra.Transfer
import qualified Lib.Finance.Domain.Types.LedgerEntry
import qualified Tools.Beam.UtilsTH

data Transfer = Transfer
  { accountId :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account,
    amount :: Kernel.Types.Common.HighPrecMoney,
    attemptCount :: Kernel.Prelude.Int,
    counterAccountId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.Account.Account),
    coveredEntryIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    createdAt :: Kernel.Prelude.UTCTime,
    currency :: Kernel.Types.Common.Currency,
    direction :: Lib.Finance.Domain.Types.Transfer.TransferDirection,
    entryIds :: Kernel.Prelude.Maybe [Kernel.Prelude.Text],
    feeAmount :: Kernel.Types.Common.HighPrecMoney,
    holdEntryId :: Kernel.Prelude.Maybe (Kernel.Types.Id.Id Lib.Finance.Domain.Types.LedgerEntry.LedgerEntry),
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.Transfer.Transfer,
    lastError :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Lib.Finance.Domain.Types.Extra.Transfer.TransferMetadata,
    railKey :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    railOrderId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    railStatus :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    referenceId :: Kernel.Prelude.Text,
    referenceType :: Kernel.Prelude.Text,
    requestedAmount :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    scheduledFor :: Kernel.Prelude.Maybe Kernel.Prelude.UTCTime,
    status :: Lib.Finance.Domain.Types.Transfer.TransferStatus,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show)

data TransferDirection = INTERNAL | OUTBOUND | INBOUND deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data TransferStatus = CREATED | SUBMITTED | IN_FLIGHT | NEEDS_REVIEW | SUCCEEDED | FAILED | REVERSED deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransferDirection))

$(mkHttpInstancesForEnum (''TransferDirection))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''TransferStatus))

$(mkHttpInstancesForEnum (''TransferStatus))
