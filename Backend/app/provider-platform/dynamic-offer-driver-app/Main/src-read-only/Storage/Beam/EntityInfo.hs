{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EntityInfo where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.DocumentReminderHistory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data EntityInfoT f = EntityInfoT
  { answer :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    entityId :: B.C f Kernel.Prelude.Text,
    entityType :: B.C f Domain.Types.DocumentReminderHistory.EntityType,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    question :: B.C f Kernel.Prelude.Text,
    questionId :: B.C f Kernel.Prelude.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EntityInfoT where
  data PrimaryKey EntityInfoT f = EntityInfoId (B.C f Kernel.Prelude.Text) (B.C f Domain.Types.DocumentReminderHistory.EntityType) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EntityInfoId <$> entityId <*> entityType <*> questionId

type EntityInfo = EntityInfoT Identity

$(enableKVPG ''EntityInfoT ['entityId, 'entityType, 'questionId] [])

$(mkTableInstances ''EntityInfoT "entity_info")
