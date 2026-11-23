module SharedLogic.EntityValueLog (boundedPage) where

import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common

maxPageSize :: Int
maxPageSize = 15

boundedPage :: MonadFlow m => Maybe Int -> Maybe Int -> m (Int, Int)
boundedPage mbLimit mbOffset = do
  when (maybe False (<= 0) mbLimit) $ throwError (InvalidRequest "limit must be positive")
  when (maybe False (< 0) mbOffset) $ throwError (InvalidRequest "offset must not be negative")
  pure (min maxPageSize (fromMaybe maxPageSize mbLimit), fromMaybe 0 mbOffset)
