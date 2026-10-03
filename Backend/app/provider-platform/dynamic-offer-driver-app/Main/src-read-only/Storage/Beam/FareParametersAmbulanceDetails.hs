{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.FareParametersAmbulanceDetails where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data FareParametersAmbulanceDetailsT f = FareParametersAmbulanceDetailsT
  { cgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    currency :: (B.C f Kernel.Types.Common.Currency),
    distBasedFare :: (B.C f Kernel.Types.Common.HighPrecMoney),
    fareParametersId :: (B.C f Kernel.Prelude.Text),
    platformFee :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney)),
    sgst :: (B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney))
  }
  deriving (Generic, B.Beamable)

instance B.Table FareParametersAmbulanceDetailsT where
  data PrimaryKey FareParametersAmbulanceDetailsT f = FareParametersAmbulanceDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = FareParametersAmbulanceDetailsId . fareParametersId

type FareParametersAmbulanceDetails = FareParametersAmbulanceDetailsT Identity

$(enableKVPG (''FareParametersAmbulanceDetailsT) [('fareParametersId)] [])

$(mkTableInstances (''FareParametersAmbulanceDetailsT) "fare_parameters_ambulance_details")
