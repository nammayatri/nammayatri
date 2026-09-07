{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.RiderPreferences
  ( findNotificationPreferenceByRiderId,
    clearNotificationPreferenceCache,
  )
where

import qualified Domain.Types.Extra.RiderPreferences as RP
import Domain.Types.Person (Person)
import qualified Domain.Types.RiderPreferences as DRP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RiderPreferences as QRP

-- Cached lookup of a rider's NOTIFICATION_PREFERENCE row (at most one per rider,
-- enforced at the application layer in Domain.Action.UI.RiderPreferences -- there is
-- no DB unique constraint). Read on every outbound notification
-- (Tools.Notifications.isNotificationCategoryAllowed), so caching this avoids a DB
-- round trip per send. The absence of a row (a legacy rider, or one who has never
-- opened the preference popup -- the common case) is cached too, not just a hit, so
-- that path also skips the DB on repeat sends rather than only speeding up riders who
-- have set a preference.
findNotificationPreferenceByRiderId :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe DRP.RiderPreferences)
findNotificationPreferenceByRiderId riderId = do
  Hedis.safeGet (makeNotificationPreferenceKey riderId) >>= \case
    Just cached -> pure cached
    Nothing -> do
      mbPref <- listToMaybe <$> QRP.findByRiderIdAndType riderId RP.NOTIFICATION_PREFERENCE
      cacheNotificationPreference riderId mbPref
      pure mbPref

cacheNotificationPreference :: (CacheFlow m r, MonadFlow m) => Id Person -> Maybe DRP.RiderPreferences -> m ()
cacheNotificationPreference riderId mbPref = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeNotificationPreferenceKey riderId) mbPref expTime

-- Call after every write to a rider's NOTIFICATION_PREFERENCE row (create or update in
-- postRiderPreference) so a stale cached value -- including a cached "no row" from
-- before the rider's first save -- never outlives the write that invalidates it.
clearNotificationPreferenceCache :: (CacheFlow m r, MonadFlow m) => Id Person -> m ()
clearNotificationPreferenceCache riderId =
  Hedis.del (makeNotificationPreferenceKey riderId)

makeNotificationPreferenceKey :: Id Person -> Text
makeNotificationPreferenceKey riderId = "CachedQueries:RiderPreferences:NotificationPreference:RiderId-" <> riderId.getId
