{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Lib.Finance.Domain.Types.AuditEntry where

import qualified Data.Aeson
import qualified Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude
import qualified Kernel.Types.Id

data AuditEntry = AuditEntry
  { action :: Lib.Finance.Domain.Types.AuditEntry.AuditAction,
    actorId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    actorType :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    entityId :: Kernel.Prelude.Text,
    entityType :: Kernel.Prelude.Text,
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

$(Kernel.Beam.Lib.UtilsTH.mkBeamInstancesForEnumAndList (''AuditAction))
