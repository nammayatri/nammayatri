{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.Person
  ( findCityInfoById,
    updateCityInfoById,
    updateCustomerTags,
    newUserTag,
    removeNewUserTag,
    findPersonStatsById,
    clearPSCache,
    clearCityInfoCache,
  )
where

import Data.List (nub)
import Domain.Action.UI.Person
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import Domain.Types.Person
import Domain.Types.PersonStats
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as YUtils
import qualified Lib.Yudhishthira.Types as YTypes
import qualified Storage.Queries.Person as Queries
import qualified Storage.Queries.PersonStats as QPS
import qualified Storage.Queries.RegistrationToken as QRT
import Tools.Auth (authTokenCacheKey, merchantIdFallback)

findCityInfoById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe PersonCityInformation)
findCityInfoById personId = do
  Hedis.safeGet (makeIdKey personId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust cachePersonCityInfo /=<< Queries.findCityInfoById personId

updateCityInfoById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> Id DM.Merchant -> City -> Id DMOC.MerchantOperatingCity -> m ()
updateCityInfoById personId merchantId city merchantOperatingCityId = do
  Queries.updateCityInfoById personId city merchantOperatingCityId
  clearCityInfoCache personId
  refreshRegistrationTokensCity personId merchantId merchantOperatingCityId

-- | The cross-cloud proxy resolves the owning cloud from the token's city, so keep
--   the person's tokens for the current merchant (and their auth cache in both
--   clouds) on the new city. Token merchants are resolved via 'merchantIdFallback',
--   as in auth, so tokens issued under a legacy merchant id still match.
refreshRegistrationTokensCity :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> m ()
refreshRegistrationTokensCity personId merchantId merchantOperatingCityId = do
  regTokens <- filter (\regToken -> merchantIdFallback (Id regToken.merchantId) == merchantId) <$> QRT.findAllByPersonId personId
  unless (null regTokens) $ do
    QRT.updateMerchantOperatingCityIdByIds ((.id) <$> regTokens) merchantOperatingCityId
    forM_ regTokens $ \regToken ->
      Hedis.runInMultiCloudRedisWrite $ Hedis.del (authTokenCacheKey regToken.token)

updateCustomerTags :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Maybe [YTypes.TagNameValueExpiry] -> Id Person -> m ()
updateCustomerTags tags personId = do
  let tagsNubbed = nub <$> tags
  Queries.updateCustomerTags tagsNubbed personId
  clearCityInfoCache personId

-- | Added on account creation, removed once the person takes their first ride or ticket.
newUserTag :: YTypes.TagNameValueExpiry
newUserTag = YTypes.TagNameValueExpiry "NewUser#Yes"

-- | Takes the person as already loaded by the caller to skip the DB read for the common
--   (non-new) case; re-reads before writing so concurrent tag updates aren't clobbered.
removeNewUserTag :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Person -> m ()
removeNewUserTag person =
  when (hasNewUserTag person.customerNammaTags) $ do
    mbLatest <- Queries.findById person.id
    whenJust mbLatest $ \latest ->
      when (hasNewUserTag latest.customerNammaTags) $ do
        updateCustomerTags (Just $ YUtils.removeTagName latest.customerNammaTags newUserTag) person.id
        logInfo $ "Removed NewUser tag for personId: " <> person.id.getId
  where
    hasNewUserTag = maybe False (YUtils.elemTagName newUserTag)

cachePersonCityInfo :: (CacheFlow m r, MonadFlow m) => PersonCityInformation -> m ()
cachePersonCityInfo personCityInfo = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makeIdKey personCityInfo.personId
      tagsNubbed = (personCityInfo {customerNammaTags = nub <$> personCityInfo.customerNammaTags} :: PersonCityInformation)
  Hedis.setExp idKey tagsNubbed expTime

clearCityInfoCache :: (CacheFlow m r, MonadFlow m) => Id Person -> m ()
clearCityInfoCache personId = do
  Hedis.runInMultiCloudRedisWrite $ Hedis.del (makeIdKey personId)

makeIdKey :: Id Person -> Text
makeIdKey personId = "CachedQueries:Person:PersonCityInformation-" <> personId.getId

findPersonStatsById :: (CacheFlow m r, EsqDBFlow m r, MonadFlow m) => Id Person -> m (Maybe PersonStats)
findPersonStatsById personId = do
  Hedis.safeGet (makePSIdKey personId) >>= \case
    Just a -> pure a
    Nothing -> flip whenJust cachePersonStats /=<< QPS.findByPersonId personId

cachePersonStats :: (CacheFlow m r, MonadFlow m) => PersonStats -> m ()
cachePersonStats personStats = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  let idKey = makePSIdKey personStats.personId
  Hedis.setExp idKey personStats expTime

clearPSCache :: (CacheFlow m r, MonadFlow m) => Id Person -> m ()
clearPSCache personId = do
  Hedis.del (makePSIdKey personId)

makePSIdKey :: Id Person -> Text
makePSIdKey personId = "CachedQueries:Person:PersonStats-" <> personId.getId
