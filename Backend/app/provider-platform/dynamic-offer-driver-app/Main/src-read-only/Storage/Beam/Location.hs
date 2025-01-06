{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Location where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data LocationT f = LocationT
  { area :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    areaCode :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    building :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    city :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    country :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    door :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    extras :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    fullAddress :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    instructions :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    state :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    street :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Kernel.Prelude.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table LocationT where
  data PrimaryKey LocationT f = LocationId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = LocationId . id

type Location = LocationT Identity

$(enableKVPG ''LocationT ['id] [])

$(mkTableInstances ''LocationT "location")
