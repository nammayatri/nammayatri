{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Stage where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Extra.Rollout
import qualified Domain.Types.Stage
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data StageT f = StageT
  { id :: B.C f Kernel.Prelude.Text,
    inputDataType :: B.C f Domain.Types.Extra.Rollout.RawDataType,
    name :: B.C f Domain.Types.Stage.StageName,
    order :: B.C f Kernel.Prelude.Int,
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table StageT where
  data PrimaryKey StageT f = StageId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StageId . id

type Stage = StageT Identity

$(enableKVPG ''StageT ['id] [])

$(mkTableInstances ''StageT "stage")
