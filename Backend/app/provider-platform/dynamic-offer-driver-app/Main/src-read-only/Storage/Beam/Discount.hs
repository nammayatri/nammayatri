{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.Discount where

import qualified Data.Aeson
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.Plan
import qualified Domain.Types.VehicleCategory
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data DiscountT f = DiscountT
  { config :: B.C f (Kernel.Prelude.Maybe Data.Aeson.Value),
    discountType :: B.C f Kernel.Prelude.Text,
    enabled :: B.C f Kernel.Prelude.Bool,
    id :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    paymentMode :: B.C f (Kernel.Prelude.Maybe Domain.Types.Plan.PaymentMode),
    planId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    validFrom :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    validTo :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.UTCTime),
    vehicleCategory :: B.C f (Kernel.Prelude.Maybe Domain.Types.VehicleCategory.VehicleCategory),
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table DiscountT where
  data PrimaryKey DiscountT f = DiscountId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = DiscountId . id

type Discount = DiscountT Identity

$(enableKVPG ''DiscountT ['id] [])

$(mkTableInstances ''DiscountT "discount")
