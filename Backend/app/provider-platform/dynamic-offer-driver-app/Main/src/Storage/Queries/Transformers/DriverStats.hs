module Storage.Queries.Transformers.DriverStats where

import GHC.Float (int2Double)
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Utils.Common (Centesimal)

getTotalDistance :: (Kernel.Types.Common.Meters -> Kernel.Prelude.Double)
getTotalDistance totalDistance = (\(Kernel.Types.Common.Meters m) -> int2Double m) totalDistance

getRating :: (Monad m) => Maybe Int -> Maybe Int -> m (Maybe Centesimal)
getRating totalRatings totalRatingScore = do
  case (totalRatings, totalRatingScore) of
    (Just tr, Just trs)
      | tr > 0 -> pure $ Just $ fromIntegral trs / fromIntegral tr
      | otherwise -> pure $ Just 0
    _ -> pure Nothing
