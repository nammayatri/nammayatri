{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ServiceCategory where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data ServiceCategoryT f = ServiceCategoryT
  { id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    description :: B.C f Kernel.Prelude.Text,
    availableSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    allowedSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    peopleCategory :: B.C f [Kernel.Prelude.Text],
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
