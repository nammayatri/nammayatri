{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.LocationMapping where

import qualified Domain.Types.Location as DL
import qualified Domain.Types.LocationMapping as DLM
import Domain.Types.Merchant (Merchant)
import Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Tools.Metrics.CoreMetrics as Metrics
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.LocationMapping as QLM

buildPickUpLocationMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.LocationMapping
buildPickUpLocationMapping locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  let order = 0
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersions entityId order
  return DLM.LocationMapping {..}

buildDropLocationMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.LocationMapping
buildDropLocationMapping locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  prevOrder <- QLM.maxOrderByEntity entityId
  let order = prevOrder + 1
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersions entityId order
  return DLM.LocationMapping {..}

buildLocationMapping' :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> Int -> m DLM.LocationMapping
buildLocationMapping' locationId entityId tag merchantId merchantOperatingCityId order = do
  id <- generateGUID
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersions entityId order
  return DLM.LocationMapping {..}

buildStopsLocationMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => [DL.Location] -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m [DLM.LocationMapping]
buildStopsLocationMapping locations entityId tag merchantId merchantOperatingCityId = do
  let order = 1
  mapM (\(location, order') -> buildStopLocationMapping location entityId tag merchantId merchantOperatingCityId order') $ zip locations [order ..]

buildStopLocationMapping :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> Int -> m DLM.LocationMapping
buildStopLocationMapping location entityId tag merchantId merchantOperatingCityId order = do
  id <- generateGUID
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
      locationId = location.id
  QLM.updatePastMappingVersions entityId order
  return DLM.LocationMapping {..}

-- ===========================================================================
-- New-entity-safe variants -- use ONLY when entityId was generated moments
-- earlier in the same flow (e.g. a fresh SearchRequest), never for edits.
-- See Storage.Queries.LocationMappingExtra.findAllByEntityIdAndOrderNewEntity
-- for why these tolerate a Postgres outage safely: a brand-new entity is
-- structurally guaranteed to have no prior mappings, so the fallback value
-- is always correct, not a guess.
-- ===========================================================================

buildPickUpLocationMappingNewEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Metrics.CoreMetrics m) => Id DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.LocationMapping
buildPickUpLocationMappingNewEntity locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  let order = 0
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersionsNewEntity entityId order
  return DLM.LocationMapping {..}

buildDropLocationMappingNewEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Metrics.CoreMetrics m) => Id DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m DLM.LocationMapping
buildDropLocationMappingNewEntity locationId entityId tag merchantId merchantOperatingCityId = do
  id <- generateGUID
  prevOrder <- QLM.maxOrderByEntityNewEntity entityId
  let order = prevOrder + 1
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
  QLM.updatePastMappingVersionsNewEntity entityId order
  return DLM.LocationMapping {..}

buildStopsLocationMappingNewEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Metrics.CoreMetrics m) => [DL.Location] -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> m [DLM.LocationMapping]
buildStopsLocationMappingNewEntity locations entityId tag merchantId merchantOperatingCityId = do
  let order = 1
  mapM (\(location, order') -> buildStopLocationMappingNewEntity location entityId tag merchantId merchantOperatingCityId order') $ zip locations [order ..]

buildStopLocationMappingNewEntity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r, Metrics.CoreMetrics m) => DL.Location -> Text -> DLM.LocationMappingTags -> Maybe (Id Merchant) -> Maybe (Id MerchantOperatingCity) -> Int -> m DLM.LocationMapping
buildStopLocationMappingNewEntity location entityId tag merchantId merchantOperatingCityId order = do
  id <- generateGUID
  now <- getCurrentTime
  let version = QLM.latestTag
      createdAt = now
      updatedAt = now
      locationId = location.id
  QLM.updatePastMappingVersionsNewEntity entityId order
  return DLM.LocationMapping {..}
