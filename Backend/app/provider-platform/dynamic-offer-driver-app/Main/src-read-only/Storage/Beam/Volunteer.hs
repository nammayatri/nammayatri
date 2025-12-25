{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Volunteer where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VolunteerT f = VolunteerT
  { createdAt :: B.C f Kernel.Prelude.UTCTime,
    id :: B.C f Data.Text.Text,
    isActive :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    place :: B.C f Data.Text.Text,
    updatedAt :: B.C f Kernel.Prelude.UTCTime,
    vendorId :: B.C f Data.Text.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table VolunteerT where
  data PrimaryKey VolunteerT f = VolunteerId (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VolunteerId <$> id <*> vendorId

type Volunteer = VolunteerT Identity

$(enableKVPG ''VolunteerT ['id, 'vendorId] [['place]])

$(mkTableInstances ''VolunteerT "volunteer")
