module Storage.Beam.Entity where

import qualified Data.Time as Time
import qualified Database.Beam as B
import Kernel.Beam.Lib.UtilsTH
import Kernel.Prelude

data EntityT f = EntityT
  { id :: B.C f Text,
    merchantId :: B.C f Text,
    entityName :: B.C f Text,
    entityShortId :: B.C f Text,
    deleted :: B.C f Bool,
    createdAt :: B.C f Time.UTCTime,
    updatedAt :: B.C f Time.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table EntityT where
  data PrimaryKey EntityT f
    = Id (B.C f Text)
    deriving (Generic, B.Beamable)
  primaryKey = Id . id

type Entity = EntityT Identity

$(enableKVPG ''EntityT ['id] [['entityShortId], ['merchantId]])

$(mkTableInstancesGenericSchema ''EntityT "entity")
