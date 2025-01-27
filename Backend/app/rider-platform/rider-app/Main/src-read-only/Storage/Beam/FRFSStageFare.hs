{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FRFSStageFare where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FRFSStageFareT f = FRFSStageFareT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Types.Common.Currency,
    farePolicyId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    stage :: B.C f Kernel.Prelude.Int,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table FRFSStageFareT where
  data PrimaryKey FRFSStageFareT f = FRFSStageFareId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Int) deriving (Generic, B.Beamable)
  primaryKey = FRFSStageFareId <$> farePolicyId <*> stage

type FRFSStageFare = FRFSStageFareT Identity

$(enableKVPG ''FRFSStageFareT ['farePolicyId, 'stage] [])

$(mkTableInstances ''FRFSStageFareT "frfs_stage_fare")
