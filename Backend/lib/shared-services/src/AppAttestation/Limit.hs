{-
  Copyright 2022-23, Juspay India Pvt Ltd

  This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

  as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

  is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

  the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module AppAttestation.Limit
  ( withinLimit,
  )
where

import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common

withinLimit :: (MonadFlow m, CacheFlow m r) => Text -> Int -> Int -> m Bool
withinLimit key limit windowSeconds = do
  now <- getCurrentTime
  let window = floor (utcTimeToPOSIXSeconds now) `div` windowSeconds :: Int
      windowKey = "AppAttestation:Limit:" <> key <> ":" <> show window
  hits <- Hedis.incr windowKey
  when (hits == 1) $ Hedis.expire windowKey windowSeconds
  pure $ hits <= fromIntegral limit
