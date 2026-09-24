{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParametersSlabDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersSlabDetailsT f = FareParametersSlabDetailsT
  { cgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    currency :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.Currency)),
    fareParametersId :: (B.C f Kernel.Prelude.Text),
    platformFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    sgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersSlabDetailsT where
  data PrimaryKey FareParametersSlabDetailsT f = FareParametersSlabDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersSlabDetailsId . fareParametersId

type FareParametersSlabDetails = FareParametersSlabDetailsT Identity

$(enableKVPG (''FareParametersSlabDetailsT) [('fareParametersId)] [])

$(mkTableInstances (''FareParametersSlabDetailsT) "fare_parameters_slab_details")
