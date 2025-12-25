module Storage.Queries.Transformers.FleetOperatorStats where

import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Storage.Queries.Transformers.DriverStats as DriverStatsTransformers

getTotalDistance :: (Kernel.Prelude.Maybe Kernel.Types.Common.Meters -> Kernel.Prelude.Maybe Kernel.Prelude.Double)
getTotalDistance = Kernel.Prelude.fmap DriverStatsTransformers.getTotalDistance
