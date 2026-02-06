{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.StopFare where

import qualified Database.Beam as B
import Domain.Types.Common ()
import qualified Domain.Types.FRFSQuoteCategoryType
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data StopFareT f = StopFareT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    bppItemId :: B.C f (Kernel.Prelude.Maybe Kernel.Prelude.Text),
    category :: B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType),
    currency :: B.C f Kernel.Types.Common.Currency,
    endStopCode :: B.C f Kernel.Prelude.Text,
    farePolicyId :: B.C f Kernel.Prelude.Text,
    integratedBppConfigId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    offeredAmount :: B.C f (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney),
    startStopCode :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table StopFareT where
  data PrimaryKey StopFareT f
    = StopFareId (B.C f (Kernel.Prelude.Maybe Domain.Types.FRFSQuoteCategoryType.FRFSQuoteCategoryType)) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text)
    deriving (Generic, B.Beamable)
  primaryKey = StopFareId <$> category <*> endStopCode <*> farePolicyId <*> startStopCode

type StopFare = StopFareT Identity

$(enableKVPG ''StopFareT ['category, 'endStopCode, 'farePolicyId, 'startStopCode] [])

$(mkTableInstances ''StopFareT "route_stop_fare")
