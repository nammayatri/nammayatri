{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.AuditEntry where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Id
import Kernel.Utils.TH
import qualified Lib.Finance.Core.Types
import qualified Tools.Beam.UtilsTH

data AuditEntry = AuditEntry
  { action :: Lib.Finance.Domain.Types.AuditEntry.AuditAction,
    actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Lib.Finance.Core.Types.ActorType,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Lib.Finance.Domain.Types.AuditEntry.AuditEntityType,
    id :: Kernel.Types.Id.Id Lib.Finance.Domain.Types.AuditEntry.AuditEntry,
    ipAddress :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    merchantId :: Kernel.Prelude.Text,
    merchantOperatingCityId :: Kernel.Prelude.Text,
    metadata :: Kernel.Prelude.Maybe Data.Aeson.Value,
    newState :: Kernel.Prelude.Maybe Data.Aeson.Value,
    previousState :: Kernel.Prelude.Maybe Data.Aeson.Value,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic)

data AuditAction = Created | Updated | Reversed | StatusChanged deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

data AuditEntityType
  = LedgerEntry
  | Invoice
  | DirectTaxTransaction
  | IndirectTaxTransaction
  | PgPaymentSettlementReport
  | SapJournalEntry
  | SubscriptionPurchase
  deriving (Eq, Ord, Show, Read, Generic, ToJSON, FromJSON, ToSchema, ToParamSchema)

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AuditAction))

$(mkHttpInstancesForEnum (''AuditAction))

$(Tools.Beam.UtilsTH.mkBeamInstancesForEnumAndList (''AuditEntityType))

$(mkHttpInstancesForEnum (''AuditEntityType))
