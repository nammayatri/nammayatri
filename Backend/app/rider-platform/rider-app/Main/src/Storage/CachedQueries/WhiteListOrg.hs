{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.WhiteListOrg
  ( findBySubscriberIdAndDomainAndMerchantId,
    countTotalSubscribers,
  )
where

import Data.Coerce (coerce)
import Domain.Types.Common
import Domain.Types.Merchant
import Domain.Types.WhiteListOrg
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Beckn.Domain (Domain (..))
import Kernel.Types.Id
import Kernel.Types.Registry (Subscriber)
import Kernel.Utils.Common
import qualified Storage.Queries.WhiteListOrg as Queries

findBySubscriberIdAndDomainAndMerchantId :: KvDbFlow m r => ShortId Subscriber -> Domain -> Id Merchant -> m (Maybe WhiteListOrg)
findBySubscriberIdAndDomainAndMerchantId subscriberId domain merchantId =
  Hedis.safeGet (makeShortIdAndDomainAndMerchantIdKey subscriberId domain merchantId) >>= \case
    Just a -> return . Just $ coerce @(WhiteListOrgD 'Unsafe) @WhiteListOrg a
    Nothing ->
      findAndCacheWithMerchantId >>= \case
        Just a' -> return $ Just a'
        -- TODO:: remove it, For backward compatibility
        Nothing -> do
          Hedis.safeGet (makeShortIdKey subscriberId domain) >>= \case
            Just a'' -> return . Just $ coerce @(WhiteListOrgD 'Unsafe) @WhiteListOrg a''
            Nothing -> findAndCache
  where
    findAndCache = flip whenJust cacheOrganization /=<< Queries.findBySubscriberIdAndDomain subscriberId domain
    findAndCacheWithMerchantId = flip whenJust cacheOrganizationWithMerchantId /=<< Queries.findBySubscriberIdAndDomainAndMerchantId subscriberId domain merchantId

cacheOrganization :: (CacheFlow m r) => WhiteListOrg -> m ()
cacheOrganization org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeShortIdKey org.subscriberId org.domain) (coerce @WhiteListOrg @(WhiteListOrgD 'Unsafe) org) expTime

makeShortIdKey :: ShortId Subscriber -> Domain -> Text
makeShortIdKey subscriberId domain = "CachedQueries:WhiteListOrg:SubscriberId-" <> subscriberId.getShortId <> "-Domain-" <> show domain

cacheOrganizationWithMerchantId :: (CacheFlow m r) => WhiteListOrg -> m ()
cacheOrganizationWithMerchantId org = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp (makeShortIdAndDomainAndMerchantIdKey org.subscriberId org.domain org.merchantId) (coerce @WhiteListOrg @(WhiteListOrgD 'Unsafe) org) expTime

makeShortIdAndDomainAndMerchantIdKey :: ShortId Subscriber -> Domain -> Id Merchant -> Text
makeShortIdAndDomainAndMerchantIdKey subscriberId domain merchantId =
  "CachedQueries:WhiteListOrg:SubscriberId-" <> subscriberId.getShortId <> "-Domain-" <> show domain <> "-MerchantId-" <> merchantId.getId

countTotalSubscribers :: KvDbFlow m r => m Int
countTotalSubscribers = do
  Hedis.safeGet "CachedQueries:WhiteListOrg:TotalSubscribers" >>= \case
    Just a -> return a
    Nothing -> findAndCache
  where
    findAndCache = cacheTotalSubscribers /=<< Queries.countTotalSubscribers

cacheTotalSubscribers :: (CacheFlow m r) => Int -> m ()
cacheTotalSubscribers orgsLen = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.setExp "CachedQueries:WhiteListOrg:TotalSubscribers" orgsLen expTime
