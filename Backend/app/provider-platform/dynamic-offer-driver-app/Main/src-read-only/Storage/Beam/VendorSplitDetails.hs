{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VendorSplitDetails where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VehicleCategory
import qualified Domain.Types.VendorSplitDetails
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Lib.Types.SpecialLocation
import Tools.Beam.UtilsTH

data VendorSplitDetailsT f = VendorSplitDetailsT
  { area :: B.C f Lib.Types.SpecialLocation.Area,
    merchantOperatingCityId :: B.C f Data.Text.Text,
    splitType :: B.C f Domain.Types.VendorSplitDetails.SplitType,
    splitValue :: B.C f Kernel.Prelude.Double,
    vehicleCategory :: B.C f Domain.Types.VehicleCategory.VehicleCategory,
    vendorId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VendorSplitDetailsT where
  data PrimaryKey VendorSplitDetailsT f
    = VendorSplitDetailsId (B.C f Lib.Types.SpecialLocation.Area) (B.C f Data.Text.Text) (B.C f Domain.Types.VehicleCategory.VehicleCategory) (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = VendorSplitDetailsId <$> area <*> merchantOperatingCityId <*> vehicleCategory <*> vendorId

type VendorSplitDetails = VendorSplitDetailsT Identity

$(enableKVPG ''VendorSplitDetailsT ['area, 'merchantOperatingCityId, 'vehicleCategory, 'vendorId] [])

$(mkTableInstances ''VendorSplitDetailsT "vendor_split_details")
