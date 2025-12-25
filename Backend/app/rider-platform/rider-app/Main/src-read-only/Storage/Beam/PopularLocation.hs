{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PopularLocation where

import qualified Data.Text
import qualified Data.Time.Clock
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data PopularLocationT f = PopularLocationT
  { address :: B.C f Data.Text.Text,
    createdAt :: B.C f Data.Time.Clock.UTCTime,
    id :: B.C f Data.Text.Text,
    lat :: B.C f Kernel.Prelude.Double,
    lon :: B.C f Kernel.Prelude.Double,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    name :: B.C f Data.Text.Text,
    rating :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Double),
    type_ :: B.C f Data.Text.Text,
    updatedAt :: B.C f Data.Time.Clock.UTCTime,
    merchantId :: B.C f (Kernel.Prelude.Maybe Data.Text.Text)
  }
  deriving (Generic, B.Beamable)

instance B.Table PopularLocationT where
  data PrimaryKey PopularLocationT f = PopularLocationId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = PopularLocationId . id

type PopularLocation = PopularLocationT Identity

$(enableKVPG ''PopularLocationT ['id] [])

$(mkTableInstances ''PopularLocationT "popular_location")
