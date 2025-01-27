{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Beam.RouteStopFare where

import qualified Database.Beam as B
import Domain.Types.Common ()
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Tools.Beam.UtilsTH

data RouteStopFareT f = RouteStopFareT
  { amount :: B.C f Kernel.Types.Common.HighPrecMoney,
    currency :: B.C f Kernel.Types.Common.Currency,
    endStopCode :: B.C f Kernel.Prelude.Text,
    farePolicyId :: B.C f Kernel.Prelude.Text,
    merchantId :: B.C f Kernel.Prelude.Text,
    merchantOperatingCityId :: B.C f Kernel.Prelude.Text,
    routeCode :: B.C f Kernel.Prelude.Text,
    startStopCode :: B.C f Kernel.Prelude.Text,
    createdAt :: B.C f Kernel.Prelude.UTCTime,
    updatedAt :: B.C f Kernel.Prelude.UTCTime
  }
  deriving (Generic, B.Beamable)

instance B.Table RouteStopFareT where
  data PrimaryKey RouteStopFareT f = RouteStopFareId (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) (B.C f Kernel.Prelude.Text) deriving (Generic, B.Beamable)
  primaryKey = RouteStopFareId <$> endStopCode <*> farePolicyId <*> routeCode <*> startStopCode

type RouteStopFare = RouteStopFareT Identity

$(enableKVPG ''RouteStopFareT ['endStopCode, 'farePolicyId, 'routeCode, 'startStopCode] [])

$(mkTableInstances ''RouteStopFareT "route_stop_fare")
