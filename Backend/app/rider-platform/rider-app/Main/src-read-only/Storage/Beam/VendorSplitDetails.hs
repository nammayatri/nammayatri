{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.VendorSplitDetails where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.VendorSplitDetails
import qualified Kernel.Prelude
import qualified Data.Text
import qualified Database.Beam as B



data VendorSplitDetailsT f
    = VendorSplitDetailsT {id :: (B.C f Data.Text.Text),
                           includeInSplit :: (B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Bool)),
                           integratedBPPConfigId :: (B.C f Data.Text.Text),
                           splitShare :: (B.C f (Kernel.Prelude.Maybe Data.Text.Text)),
                           splitType :: (B.C f Domain.Types.VendorSplitDetails.SplitType),
                           vendorId :: (B.C f Data.Text.Text),
                           merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
                           createdAt :: (B.C f Kernel.Prelude.UTCTime),
                           updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table VendorSplitDetailsT
    where data PrimaryKey VendorSplitDetailsT f = VendorSplitDetailsId (B.C f Data.Text.Text) deriving (Generic, B.Beamable)
          primaryKey = VendorSplitDetailsId . id
type VendorSplitDetails = VendorSplitDetailsT Identity

$(enableKVPG (''VendorSplitDetailsT) [('id)] [])

$(mkTableInstances (''VendorSplitDetailsT) "vendor_split_details")

