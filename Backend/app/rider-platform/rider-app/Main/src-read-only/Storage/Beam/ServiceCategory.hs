{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.ServiceCategory where

import qualified Database.Beam as B
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceCategory
import qualified Domain.Types.ServicePeopleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data ServiceCategoryT f = ServiceCategoryT
  { allowedSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    availableSeats :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Int),
    description :: B.C f Kernel.Prelude.Text,
    id :: B.C f Kernel.Prelude.Text,
    name :: B.C f Kernel.Prelude.Text,
    peopleCategory :: B.C f [Kernel.Prelude.Text],
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table ServiceCategoryT where
  data PrimaryKey ServiceCategoryT f = ServiceCategoryId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = ServiceCategoryId . id

type ServiceCategory = ServiceCategoryT Identity

$(enableKVPG ''ServiceCategoryT ['id] [])

$(mkTableInstances ''ServiceCategoryT "service_category")
