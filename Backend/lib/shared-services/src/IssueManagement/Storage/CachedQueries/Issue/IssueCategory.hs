{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
module IssueManagement.Storage.CachedQueries.Issue.IssueCategory where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueTranslation
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueCategory as Queries
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)

findAllActiveByMerchantOpCityIdAndLanguage :: BeamFlow m r => Id MerchantOperatingCity -> Language -> Identifier -> m [(IssueCategory, Maybe IssueTranslation)]
findAllActiveByMerchantOpCityIdAndLanguage merchantOpCityId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByMerchantOpCityIdAndLanguageKey merchantOpCityId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueCategoryByMerchantOpCityIdAndLanguage merchantOpCityId language identifier /=<< Queries.findAllActiveByMerchantOpCityIdAndLanguage merchantOpCityId language

findById :: BeamFlow m r => Id IssueCategory -> Identifier -> m (Maybe IssueCategory)
findById issueCategoryId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdKey issueCategoryId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryById issueCategoryId identifier /=<< Queries.findById issueCategoryId

findByIdAndLanguage :: BeamFlow m r => Id IssueCategory -> Language -> Identifier -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage issueCategoryId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryByIdAndLanguage issueCategoryId language identifier /=<< Queries.findByIdAndLanguage issueCategoryId language

updateByPrimaryKey :: BeamFlow m r => IssueCategory -> m ()
updateByPrimaryKey = Queries.updateByPrimaryKey

--------- Caching logic for issue category by merchant operating city id and language -------------------

clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache :: BeamFlow m r => Id MerchantOperatingCity -> Identifier -> m ()
clearAllIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOpCityId identifier = forM_ allLanguages $ \language ->
  clearIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOpCityId language identifier

clearIssueCategoryByMerchantOpCityIdAndLanguageCache :: BeamFlow m r => Id MerchantOperatingCity -> Language -> Identifier -> m ()
clearIssueCategoryByMerchantOpCityIdAndLanguageCache merchantOpCityId language identifier = do
  Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByMerchantOpCityIdAndLanguageKey merchantOpCityId language identifier

cacheAllIssueCategoryByMerchantOpCityIdAndLanguage :: CacheFlow m r => Id MerchantOperatingCity -> Language -> Identifier -> [(IssueCategory, Maybe IssueTranslation)] -> m ()
cacheAllIssueCategoryByMerchantOpCityIdAndLanguage merchantOpCityId language identifier issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByMerchantOpCityIdAndLanguageKey merchantOpCityId language identifier) issueCategoryTranslation expTime

makeIssueCategoryByMerchantOpCityIdAndLanguageKey :: Id MerchantOperatingCity -> Language -> Identifier -> Text
makeIssueCategoryByMerchantOpCityIdAndLanguageKey merchantOpCityId language identifier = show identifier <> ":CachedQueries:IssueCategory:MerchantOpCityId-" <> merchantOpCityId.getId <> ":Language-" <> show language

--------- Caching logic for issue category by id -------------------

clearIssueCategoryByIdCache :: CacheFlow m r => Id IssueCategory -> Identifier -> m ()
clearIssueCategoryByIdCache issueCategoryId identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByIdKey issueCategoryId identifier

cacheIssueCategoryById :: CacheFlow m r => Id IssueCategory -> Identifier -> Maybe IssueCategory -> m ()
cacheIssueCategoryById issueCategoryId identifier issueCategory = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdKey issueCategoryId identifier) issueCategory expTime

makeIssueCategoryByIdKey :: Id IssueCategory -> Identifier -> Text
makeIssueCategoryByIdKey id identifier = show identifier <> ":CachedQueries:IssueCategory:Id-" <> id.getId

--------- Caching logic for issue category by id and language -------------------

clearAllIssueCategoryByIdAndLanguageCache :: CacheFlow m r => Id IssueCategory -> Identifier -> m ()
clearAllIssueCategoryByIdAndLanguageCache issueCategoryId identifier = forM_ allLanguages $ \language ->
  clearIssueCategoryByIdAndLanguageCache issueCategoryId language identifier

clearIssueCategoryByIdAndLanguageCache :: (CacheFlow m r) => Id IssueCategory -> Language -> Identifier -> m ()
clearIssueCategoryByIdAndLanguageCache issueCategoryId language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier

cacheIssueCategoryByIdAndLanguage :: (CacheFlow m r) => Id IssueCategory -> Language -> Identifier -> Maybe (IssueCategory, Maybe IssueTranslation) -> m ()
cacheIssueCategoryByIdAndLanguage issueCategoryId language identifier issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier) issueCategoryTranslation expTime

makeIssueCategoryByIdAndLanguageKey :: Id IssueCategory -> Language -> Identifier -> Text
makeIssueCategoryByIdAndLanguageKey id language identifier = show identifier <> ":CachedQueries:IssueCategory:Id-" <> id.getId <> ":Language-" <> show language
