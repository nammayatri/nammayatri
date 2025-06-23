{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.SpecialOccasion where

import qualified Data.Time
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.SpecialOccasion
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data SpecialOccasionT f = SpecialOccasionT
  { businessHours :: B.C f [Kernel.Prelude.Text],
    date :: B.C f (Kernel.Prelude.Maybe Data.Time.Day),
    dayOfWeek :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    description :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    entityId :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    placeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    specialDayType :: B.C f Domain.Types.SpecialOccasion.SpecialDayType,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table SpecialOccasionT where
  data PrimaryKey SpecialOccasionT f = SpecialOccasionId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = SpecialOccasionId . id

type SpecialOccasion = SpecialOccasionT Identity

$(enableKVPG ''SpecialOccasionT ['id] [['entityId]])

$(mkTableInstances ''SpecialOccasionT "special_occasion")
