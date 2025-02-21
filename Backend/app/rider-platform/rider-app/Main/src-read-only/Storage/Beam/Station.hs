{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Station where

import qualified BecknV2.FRFS.Enums
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.StationType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.TimeBound
import Tools.Beam.UtilsTH

data StationT f = StationT
  { address :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    code :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    lat :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    lon :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    possibleTypes :: B.C f (Kernel.Prelude.Maybe [Domain.Types.StationType.StationType]),
    timeBounds :: B.C f (Kernel.Prelude.Maybe Kernel.Types.TimeBound.TimeBound),
    vehicleType :: B.C f BecknV2.FRFS.Enums.VehicleCategory,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table StationT where
  data PrimaryKey StationT f = StationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = StationId . id

type Station = StationT Identity

$(enableKVPG ''StationT ['id] [['code]])

$(mkTableInstances ''StationT "station")
