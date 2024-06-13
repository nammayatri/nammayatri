{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Transformers.DriverStats where

import GHC.Float (int2Double)
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, Centesimal, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)

getTotalDistance :: (Kernel.Types.Common.Meters -> Kernel.Prelude.Double)
getTotalDistance totalDistance = (\(Kernel.Types.Common.Meters m) -> int2Double m) totalDistance

getRating :: (Monad m) => Maybe Int -> Maybe Int -> m (Maybe Centesimal)
getRating totalRatings totalRatingScore = do
  case (totalRatings, totalRatingScore) of
    (Just tr, Just trs)
      | tr > 0 -> pure $ Just $ fromIntegral trs / fromIntegral tr
      | otherwise -> pure $ Just 0
    _ -> pure Nothing
