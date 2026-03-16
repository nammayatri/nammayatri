{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Transformers.FleetRcDailyStats where

import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Storage.Queries.Transformers.DriverStats as DriverStatsTransformers

getRideDistance :: (Kernel.Types.Common.Meters -> Kernel.Prelude.Double)
getRideDistance = DriverStatsTransformers.getTotalDistance
