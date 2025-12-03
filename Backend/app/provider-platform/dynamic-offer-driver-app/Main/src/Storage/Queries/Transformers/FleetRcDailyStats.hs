{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.FleetRcDailyStats where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Queries.Transformers.DriverStats as DriverStatsTransformers

getRideDistance :: (Kernel.Types.Common.Meters -> Kernel.Prelude.Double)
getRideDistance = DriverStatsTransformers.getTotalDistance
