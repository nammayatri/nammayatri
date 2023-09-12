{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Storage.CachedQueries.Issue.IssueOption where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueTranslation
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow)
import qualified Storage.Queries.Issue.IssueOption as Queries

findAllByCategoryAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage issueCategoryId language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueOptionByCategoryAndLanguageKey issueCategoryId language) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueOptionByCategoryAndLanguage issueCategoryId language /=<< Queries.findAllByCategoryAndLanguage issueCategoryId language

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueOption -> m (Maybe IssueOption)
findById issueOptionId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueOptionByIdKey issueOptionId) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueOptionById issueOptionId /=<< Queries.findById issueOptionId

findByIdAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueOptionByIdAndLanguageKey issueOptionId language) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueOptionByIdAndLanguage issueOptionId language /=<< Queries.findByIdAndLanguage issueOptionId language

findByIdAndCategoryId :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueOptionByIdAndIssueCategoryIdKey issueOptionId issueCategoryId) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueOptionByIdAndIssueCategoryId issueOptionId issueCategoryId /=<< Queries.findByIdAndCategoryId issueOptionId issueCategoryId

--------- Caching logic for issue option by issueCategoryId & language -------------------

clearIssueOptionByCategoryAndLanguageCache :: (CacheFlow m r) => Id IssueCategory -> Language -> m ()
clearIssueOptionByCategoryAndLanguageCache issueCategoryId = Hedis.withCrossAppRedis . Hedis.del . makeIssueOptionByCategoryAndLanguageKey issueCategoryId

cacheAllIssueOptionByCategoryAndLanguage :: (CacheFlow m r) => Id IssueCategory -> Language -> [(IssueOption, Maybe IssueTranslation)] -> m ()
cacheAllIssueOptionByCategoryAndLanguage issueCategoryId language issueOptionTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueOptionByCategoryAndLanguageKey issueCategoryId language) issueOptionTranslation expTime

makeIssueOptionByCategoryAndLanguageKey :: Id IssueCategory -> Language -> Text
makeIssueOptionByCategoryAndLanguageKey issueCategoryId language = "driver-offer:CachedQueries:IssueOption:IssueCategoryId-" <> getId issueCategoryId <> ":Language-" <> show language

--------- Caching logic for issue option by id -------------------

clearIssueOptionByIdCache :: (CacheFlow m r) => Id IssueOption -> m ()
clearIssueOptionByIdCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueOptionByIdKey

cacheIssueOptionById :: (CacheFlow m r) => Id IssueOption -> Maybe IssueOption -> m ()
cacheIssueOptionById issueOptionId issueOption = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueOptionByIdKey issueOptionId) issueOption expTime

makeIssueOptionByIdKey :: Id IssueOption -> Text
makeIssueOptionByIdKey id = "driver-offer:CachedQueries:IssueOption:Id-" <> show id

--------- Caching logic for issue option by id and language -------------------

clearIssueOptionByIdAndLanguageCache :: (CacheFlow m r) => Id IssueOption -> Language -> m ()
clearIssueOptionByIdAndLanguageCache issueOptionId = Hedis.withCrossAppRedis . Hedis.del . makeIssueOptionByIdAndLanguageKey issueOptionId

cacheIssueOptionByIdAndLanguage :: (CacheFlow m r) => Id IssueOption -> Language -> Maybe (IssueOption, Maybe IssueTranslation) -> m ()
cacheIssueOptionByIdAndLanguage issueOptionId language issueOptionTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueOptionByIdAndLanguageKey issueOptionId language) issueOptionTranslation expTime

makeIssueOptionByIdAndLanguageKey :: Id IssueOption -> Language -> Text
makeIssueOptionByIdAndLanguageKey id language = "driver-offer:CachedQueries:IssueOption:Id-" <> show id <> ":Language-" <> show language

--------- Caching logic for issue option by id and issueCategoryId -------------------

clearIssueOptionByIdAndIssueCategoryIdCache :: (CacheFlow m r) => Id IssueOption -> Id IssueCategory -> m ()
clearIssueOptionByIdAndIssueCategoryIdCache issueOptionId = Hedis.withCrossAppRedis . Hedis.del . makeIssueOptionByIdAndIssueCategoryIdKey issueOptionId

cacheIssueOptionByIdAndIssueCategoryId :: (CacheFlow m r) => Id IssueOption -> Id IssueCategory -> Maybe IssueOption -> m ()
cacheIssueOptionByIdAndIssueCategoryId issueOptionId issueCategoryId issueOptionTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueOptionByIdAndIssueCategoryIdKey issueOptionId issueCategoryId) issueOptionTranslation expTime

makeIssueOptionByIdAndIssueCategoryIdKey :: Id IssueOption -> Id IssueCategory -> Text
makeIssueOptionByIdAndIssueCategoryIdKey id issueCategoryId = "driver-offer:CachedQueries:IssueOption:Id-" <> show id <> ":IssueCategoryId-" <> show issueCategoryId
