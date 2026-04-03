{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VendorSplitDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Lib.Types.SpecialLocation
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Domain.Types.VendorSplitDetails
import qualified Domain.Types.VehicleVariant
import qualified Data.Text
import qualified Database.Beam as B



data VendorSplitDetailsT f
    = VendorSplitDetailsT {area :: (B.C f Lib.Types.SpecialLocation.Area),
                           maxVendorFeeAmount :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                           merchantOperatingCityId :: (B.C f Data.Text.Text),
                           splitType :: (B.C f Domain.Types.VendorSplitDetails.SplitType),
                           splitValue :: (B.C f Kernel.Prelude.Double),
                           vehicleVariant :: (B.C f Domain.Types.VehicleVariant.VehicleVariant),
                           vendorId :: (B.C f Data.Text.Text),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VendorSplitDetailsT
    where data PrimaryKey VendorSplitDetailsT f
              = VendorSplitDetailsId (B.C f Lib.Types.SpecialLocation.Area) (B.C f Data.Text.Text) (B.C f Domain.Types.VehicleVariant.VehicleVariant) (B.C f Data.Text.Text)
              deriving (Generic, B.Beamable)
          primaryKey = VendorSplitDetailsId <$> area <*> merchantOperatingCityId <*> vehicleVariant <*> vendorId
type VendorSplitDetails = VendorSplitDetailsT Identity

$(enableKVPG (''VendorSplitDetailsT) [('area), ('merchantOperatingCityId), ('vehicleVariant), ('vendorId)] [])

$(mkTableInstances (''VendorSplitDetailsT) "vendor_split_details")

