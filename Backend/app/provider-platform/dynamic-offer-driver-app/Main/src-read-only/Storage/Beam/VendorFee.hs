{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VendorFee where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Kernel.Types.Common
import qualified Data.Text
import qualified Kernel.Prelude
import qualified Database.Beam as B



data VendorFeeT f
    = VendorFeeT {amount :: (B.C f Kernel.Types.Common.HighPrecMoney),
                  driverFeeId :: (B.C f Data.Text.Text),
                  vendorId :: (B.C f Data.Text.Text),
                  createdAt :: (B.C f Kernel.Prelude.UTCTime),
                  updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VendorFeeT
    where data PrimaryKey VendorFeeT f = VendorFeeId (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = VendorFeeId <$> driverFeeId <*> vendorId
type VendorFee = VendorFeeT Identity

$(enableKVPG (''VendorFeeT) [('driverFeeId), ('vendorId)] [])

$(mkTableInstances (''VendorFeeT) "vendor_fee")

