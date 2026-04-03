{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE StandaloneDeriving #-}
module Storage.Beam.FRFSFarePolicy where
import Kernel.Prelude
import Tools.Beam.UtilsTH
import Kernel.External.Encryption
import Domain.Types.Common ()
import qualified Domain.Types.FRFSFarePolicy
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Database.Beam as B



data FRFSFarePolicyT f
    = FRFSFarePolicyT {_type :: (B.C f Domain.Types.FRFSFarePolicy.FRFSFarePolicyType),
                       applicableDiscountIds :: (B.C f [Kernel.Prelude.Text]),
                       cessCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
                       description :: (B.C f Kernel.Prelude.Text),
                       id :: (B.C f Kernel.Prelude.Text),
                       merchantId :: (B.C f Kernel.Prelude.Text),
                       merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
                       createdAt :: (B.C f Kernel.Prelude.UTCTime),
                       updatedAt :: (B.C f Kernel.Prelude.UTCTime)}
    deriving (Generic, B.Beamable)
instance B.Table FRFSFarePolicyT
    where data PrimaryKey FRFSFarePolicyT f = FRFSFarePolicyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
          primaryKey = FRFSFarePolicyId . id
type FRFSFarePolicy = FRFSFarePolicyT Identity

$(enableKVPG (''FRFSFarePolicyT) [('id)] [])

$(mkTableInstances (''FRFSFarePolicyT) "frfs_fare_policy")

