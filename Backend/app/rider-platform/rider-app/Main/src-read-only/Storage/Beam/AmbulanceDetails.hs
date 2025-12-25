{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.AmbulanceDetails where

import qualified Database.Beam as B
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Kernel.Utils.Common
import Tools.Beam.UtilsTH

data AmbulanceDetailsT f = AmbulanceDetailsT
  { id :: B.C f Kernel.Prelude.Text,
    maxEstimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Utils.Common.Currency,
    minEstimatedFare :: B.C f Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, B.Beamable)

instance B.Table AmbulanceDetailsT where
  data PrimaryKey AmbulanceDetailsT f = AmbulanceDetailsId (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = AmbulanceDetailsId . id

type AmbulanceDetails = AmbulanceDetailsT Identity

$(enableKVPG ''AmbulanceDetailsT ['id] [])

$(mkTableInstances ''AmbulanceDetailsT "ambulance_details")
