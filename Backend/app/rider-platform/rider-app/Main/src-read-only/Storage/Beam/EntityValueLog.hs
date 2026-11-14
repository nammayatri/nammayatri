{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.EntityValueLog where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.EntityValueLog
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.ActorInfo
import Tools.Beam.UtilsTH

data EntityValueLogT f = EntityValueLogT
  { actorId :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    actorType :: (B.C f Kernel.Types.ActorInfo.ActorType),
    delta :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    entityId :: (B.C f Kernel.Prelude.Text),
    entityType :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    operationId :: (B.C f Kernel.Prelude.Text),
    operationType :: (B.C f Domain.Types.EntityValueLog.OperationType),
    reason :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)),
    status :: (B.C f Domain.Types.EntityValueLog.OperationStatus),
    valueAfter :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    valueBefore :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int)),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table EntityValueLogT where
  data PrimaryKey EntityValueLogT f = EntityValueLogId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = EntityValueLogId . id

type EntityValueLog = EntityValueLogT Identity

$(enableKVPG (''EntityValueLogT) [('id)] [[('entityId)]])

$(mkTableInstances (''EntityValueLogT) "entity_value_log")
