{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.PayoutInstanceCalculation where

import qualified Data.Text
import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data PayoutInstanceCalculationT f = PayoutInstanceCalculationT
  { endTime :: (B.C f Kernel.Prelude.UTCTime),
    fromVendorId :: (B.C f Data.Text.Text),
    id :: (B.C f Data.Text.Text),
    instanceBalance :: (B.C f Kernel.Types.Common.HighPrecMoney),
    startTime :: (B.C f Kernel.Prelude.UTCTime),
    toVendorId :: (B.C f Data.Text.Text),
    merchantId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    merchantOperatingCityId :: (B.C f (Kernel.Prelude.Maybe (Data.Text.Text))),
    createdAt :: (B.C f Kernel.Prelude.UTCTime),
    updatedAt :: (B.C f Kernel.Prelude.UTCTime)
  }
  deriving (Generic, B.Beamable)

instance B.Table PayoutInstanceCalculationT where
  data PrimaryKey PayoutInstanceCalculationT f
    = PayoutInstanceCalculationId (B.C f Kernel.Prelude.UTCTime) (B.C f Data.Text.Text) (B.C f Data.Text.Text) (B.C f Kernel.Prelude.UTCTime) (B.C f Data.Text.Text)
    deriving (Generic, B.Beamable)
  primaryKey = PayoutInstanceCalculationId <$> endTime <*> fromVendorId <*> id <*> startTime <*> toVendorId

type PayoutInstanceCalculation = PayoutInstanceCalculationT Identity

$(enableKVPG (''PayoutInstanceCalculationT) [('endTime), ('fromVendorId), ('id), ('startTime), ('toVendorId)] [])

$(mkTableInstances (''PayoutInstanceCalculationT) "payout_instance_calculation")
