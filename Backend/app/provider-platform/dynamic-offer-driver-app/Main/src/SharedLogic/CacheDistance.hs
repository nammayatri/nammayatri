{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.CacheDistance where

import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common

cacheDistance ::
  (Redis.HedisFlow m r, MonadTime m) =>
  MonadFlow m =>
  Text ->
  (Meters, Seconds) ->
  m ()
cacheDistance transactionId distance =
  Redis.setExp (distanceKey transactionId) distance 7200

getCacheDistance ::
  (Redis.HedisFlow m r) =>
  Text ->
  m (Maybe (Meters, Seconds))
getCacheDistance transactionId = Redis.get @(Meters, Seconds) (distanceKey transactionId)

distanceKey :: Text -> Text
distanceKey = ("distanceKey:" <>)
