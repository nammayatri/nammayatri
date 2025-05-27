{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ServiceCategory where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ServiceCategoryT f = ServiceCategoryT
  { allowedSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    availableSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    isClosed :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool),
    name :: B.C f Kernel.Prelude.Text,
    peopleCategory :: B.C f [Kernel.Prelude.Text],
    placeId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    rules :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    merchantId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ServiceCategoryT where
  data PrimaryKey ServiceCategoryT f = ServiceCategoryId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = ServiceCategoryId . id

type ServiceCategory = ServiceCategoryT Identity

$(enableKVPG ''ServiceCategoryT ['id] [])

$(mkTableInstances ''ServiceCategoryT "service_category")
