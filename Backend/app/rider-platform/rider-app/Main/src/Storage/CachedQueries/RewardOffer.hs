{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.RewardOffer
  ( findAllActiveByMerchantOpCityId,
    findById,
    clearCache,
    clearCacheById,
  )
where

import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.RewardOffer as DReward
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.RewardOffer as Queries

findAllActiveByMerchantOpCityId ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  m [DReward.RewardOffer]
findAllActiveByMerchantOpCityId merchantOpCityId =
  Hedis.safeGet (makeActiveOffersByMerchantOpCityIdKey merchantOpCityId) >>= \case
    Just offers -> pure offers
    Nothing ->
      cacheActiveOffersByMerchantOpCityId merchantOpCityId
        /=<< Queries.findAllActiveByMerchantOpCityId merchantOpCityId

findById ::
  (CacheFlow m r, MonadFlow m, EsqDBFlow m r) =>
  Id DReward.RewardOffer ->
  m (Maybe DReward.RewardOffer)
findById offerId =
  Hedis.safeGet (makeRewardOfferByIdKey offerId) >>= \case
    Just offer -> pure offer
    Nothing -> do
      offer <- Queries.findById offerId
      expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
      Hedis.setExp (makeRewardOfferByIdKey offerId) offer expTime
      pure offer

cacheActiveOffersByMerchantOpCityId ::
  (CacheFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  [DReward.RewardOffer] ->
  m ()
cacheActiveOffersByMerchantOpCityId merchantOpCityId offers = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeActiveOffersByMerchantOpCityIdKey merchantOpCityId) offers expTime

clearCache :: (CacheFlow m r, MonadFlow m) => Id DMOC.MerchantOperatingCity -> m ()
clearCache merchantOpCityId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.del (makeActiveOffersByMerchantOpCityIdKey merchantOpCityId)

clearCacheById :: (CacheFlow m r, MonadFlow m) => Id DReward.RewardOffer -> m ()
clearCacheById offerId =
  Hedis.runInMultiCloudRedisWrite $
    Hedis.del (makeRewardOfferByIdKey offerId)

makeActiveOffersByMerchantOpCityIdKey :: Id DMOC.MerchantOperatingCity -> Text
makeActiveOffersByMerchantOpCityIdKey merchantOpCityId =
  "rider-platform:CachedQueries:RewardOffer:Active:MerchantOpCityId-" <> merchantOpCityId.getId

makeRewardOfferByIdKey :: Id DReward.RewardOffer -> Text
makeRewardOfferByIdKey offerId =
  "rider-platform:CachedQueries:RewardOffer:Id-" <> offerId.getId
