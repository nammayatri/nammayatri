{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VendorFee where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data VendorFeeT f = VendorFeeT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    driverFeeId :: B.C f Data.Text.Text,
    vendorId :: B.C f Data.Text.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table VendorFeeT where
  data PrimaryKey VendorFeeT f = VendorFeeId (B.C f Data.Text.Text) (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VendorFeeId <$> driverFeeId <*> vendorId

type VendorFee = VendorFeeT Identity

$(enableKVPG ''VendorFeeT ['driverFeeId, 'vendorId] [])

$(mkTableInstances ''VendorFeeT "vendor_fee")
