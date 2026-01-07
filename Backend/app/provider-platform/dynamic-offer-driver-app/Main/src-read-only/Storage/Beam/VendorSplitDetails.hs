{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VendorSplitDetails where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleVariant
import qualified Domain.Types.VendorSplitDetails
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data VendorSplitDetailsT f = VendorSplitDetailsT
  { area :: B.C f Lib.Types.SpecialLocation.Area,
    maxVendorFeeAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    merchantOperatingCityId :: B.C f Data.Text.Text,
    splitType :: B.C f Domain.Types.VendorSplitDetails.SplitType,
    splitValue :: B.C f Kernel.Prelude.Double,
    vehicleVariant :: B.C f Domain.Types.VehicleVariant.VehicleVariant,
    vendorId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VendorSplitDetailsT where
  data PrimaryKey VendorSplitDetailsT f
    = VendorSplitDetailsId (B.C f Lib.Types.SpecialLocation.Area) (B.C f Data.Text.Text) (B.C f Domain.Types.VehicleVariant.VehicleVariant) (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = VendorSplitDetailsId <$> area <*> merchantOperatingCityId <*> vehicleVariant <*> vendorId

type VendorSplitDetails = VendorSplitDetailsT Identity

$(enableKVPG ''VendorSplitDetailsT ['area, 'merchantOperatingCityId, 'vehicleVariant, 'vendorId] [])

$(mkTableInstances ''VendorSplitDetailsT "vendor_split_details")
