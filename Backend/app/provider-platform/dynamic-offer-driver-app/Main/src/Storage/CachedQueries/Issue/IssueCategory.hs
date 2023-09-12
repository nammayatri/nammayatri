{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Issue.IssueCategory where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.Issue.IssueCategory as Queries

findAllByLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByLanguageKey language) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueCategoryByLanguage language /=<< Queries.findAllByLanguage language

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueCategory -> m (Maybe IssueCategory)
findById issueCategoryId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdKey issueCategoryId) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryById issueCategoryId /=<< Queries.findById issueCategoryId

findByIdAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage issueCategoryId language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdAndLanguageKey issueCategoryId language) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryByIdAndLanguage issueCategoryId language /=<< Queries.findByIdAndLanguage issueCategoryId language

--------- Caching logic for issue category by language -------------------

clearIssueCategoryByLanguageCache :: (CacheFlow m r) => Language -> m ()
clearIssueCategoryByLanguageCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueCategoryByLanguageKey

cacheAllIssueCategoryByLanguage :: (CacheFlow m r) => Language -> [(IssueCategory, Maybe IssueTranslation)] -> m ()
cacheAllIssueCategoryByLanguage language issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByLanguageKey language) issueCategoryTranslation expTime

makeIssueCategoryByLanguageKey :: Language -> Text
makeIssueCategoryByLanguageKey language = "driver-offer:CachedQueries:IssueCategory:Language-" <> show language

--------- Caching logic for issue category by id -------------------

clearIssueCategoryByIdCache :: (CacheFlow m r) => Id IssueCategory -> m ()
clearIssueCategoryByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueCategoryByIdKey

cacheIssueCategoryById :: (CacheFlow m r) => Id IssueCategory -> Maybe IssueCategory -> m ()
cacheIssueCategoryById issueCategoryId issueCategory = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdKey issueCategoryId) issueCategory expTime

makeIssueCategoryByIdKey :: Id IssueCategory -> Text
makeIssueCategoryByIdKey id = "driver-offer:CachedQueries:IssueCategory:Id-" <> show id

--------- Caching logic for issue category by id -------------------

clearIssueCategoryByIdAndLanguageCache :: (CacheFlow m r) => Id IssueCategory -> Language -> m ()
clearIssueCategoryByIdAndLanguageCache issueCategoryId = Hedis.withCrossAppRedis . Hedis.del . makeIssueCategoryByIdAndLanguageKey issueCategoryId

cacheIssueCategoryByIdAndLanguage :: (CacheFlow m r) => Id IssueCategory -> Language -> Maybe (IssueCategory, Maybe IssueTranslation) -> m ()
cacheIssueCategoryByIdAndLanguage issueCategoryId language issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdAndLanguageKey issueCategoryId language) issueCategoryTranslation expTime

makeIssueCategoryByIdAndLanguageKey :: Id IssueCategory -> Language -> Text
makeIssueCategoryByIdAndLanguageKey id language = "driver-offer:CachedQueries:IssueCategory:Id-" <> show id <> ":Language-" <> show language
