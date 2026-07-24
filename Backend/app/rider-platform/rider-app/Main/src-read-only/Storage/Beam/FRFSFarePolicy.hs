{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSFarePolicy where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSFarePolicy
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSFarePolicyT f = FRFSFarePolicyT
  { _type :: (B.C f Domain.Types.FRFSFarePolicy.FRFSFarePolicyType),
    applicableDiscountIds :: (B.C f [Kernel.Prelude.Text]),
    cessCharge :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    description :: (B.C f Kernel.Prelude.Text),
    id :: (B.C f Kernel.Prelude.Text),
    merchantId :: (B.C f Kernel.Prelude.Text),
    merchantOperatingCityId :: (B.C f Kernel.Prelude.Text),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSFarePolicyT where
  data PrimaryKey FRFSFarePolicyT f = FRFSFarePolicyId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FRFSFarePolicyId . id

type FRFSFarePolicy = FRFSFarePolicyT Identity

$(enableKVPG (''FRFSFarePolicyT) [('id)] [])

$(mkTableInstances (''FRFSFarePolicyT) "frfs_fare_policy")
