{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.VendorSplitDetails where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.VendorSplitDetails
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Tools.Beam.UtilsTH

data VendorSplitDetailsT f = VendorSplitDetailsT
  { id :: (B.C f Data.Text.Text),
    integratedBPPConfigId :: (B.C f Data.Text.Text),
    splitType :: (B.C f Domain.Types.VendorSplitDetails.SplitType),
    vendorId :: (B.C f Data.Text.Text),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table VendorSplitDetailsT where
  data PrimaryKey VendorSplitDetailsT f = VendorSplitDetailsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
  primaryKey = VendorSplitDetailsId . id

type VendorSplitDetails = VendorSplitDetailsT Identity

$(enableKVPG (''VendorSplitDetailsT) [('id)] [])

$(mkTableInstances (''VendorSplitDetailsT) "vendor_split_details")
