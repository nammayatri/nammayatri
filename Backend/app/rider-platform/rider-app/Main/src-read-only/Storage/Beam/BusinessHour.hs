{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.BusinessHour where

import qualified Database.Beam as B
import qualified Domain.Types.BusinessHour
import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantOperatingCity
import qualified Domain.Types.ServiceCategory
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Id
import Tools.Beam.UtilsTH

data BusinessHourT f = BusinessHourT
  { btype :: B.C f Domain.Types.BusinessHour.BusinessHourType,
    categoryId :: B.C f [Kernel.Prelude.Text],
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    merchantOperatingCityId :: B.C f (Kernel.Prelude.Maybe (Kernel.Prelude.Text)),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table BusinessHourT where
  data PrimaryKey BusinessHourT f = BusinessHourId (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = BusinessHourId . id

type BusinessHour = BusinessHourT Identity

$(enableKVPG ''BusinessHourT ['id] [])

$(mkTableInstances ''BusinessHourT "business_hour")
