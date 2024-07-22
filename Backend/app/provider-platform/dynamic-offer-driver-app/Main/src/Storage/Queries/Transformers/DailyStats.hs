{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DailyStats where

import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getHighPrecMoney :: (Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney -> Kernel.Types.Common.HighPrecMoney)
getHighPrecMoney = fromMaybe 0.0
