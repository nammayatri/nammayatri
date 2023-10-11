{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# LANGUAGE DerivingStrategies #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

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

findAllByLanguage :: (CacheFlow m r, BeamFlow m) => Language -> Identifier -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByLanguageKey language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueCategoryByLanguage language identifier /=<< Queries.findAllByLanguage language

findById :: (CacheFlow m r, BeamFlow m) => Id IssueCategory -> Identifier -> m (Maybe IssueCategory)
findById issueCategoryId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdKey issueCategoryId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryById issueCategoryId identifier /=<< Queries.findById issueCategoryId

findByIdAndLanguage :: (CacheFlow m r, BeamFlow m) => Id IssueCategory -> Language -> Identifier -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage issueCategoryId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategoryByIdAndLanguage issueCategoryId language identifier /=<< Queries.findByIdAndLanguage issueCategoryId language

--------- Caching logic for issue category by language -------------------

clearIssueCategoryByLanguageCache :: (CacheFlow m r) => Language -> Identifier -> m ()
clearIssueCategoryByLanguageCache language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByLanguageKey language identifier

cacheAllIssueCategoryByLanguage :: (CacheFlow m r) => Language -> Identifier -> [(IssueCategory, Maybe IssueTranslation)] -> m ()
cacheAllIssueCategoryByLanguage language identifier issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByLanguageKey language identifier) issueCategoryTranslation expTime

makeIssueCategoryByLanguageKey :: Language -> Identifier -> Text
makeIssueCategoryByLanguageKey language identifier = show identifier <> "CachedQueries:IssueCategory:Language-" <> show language

--------- Caching logic for issue category by id -------------------

clearIssueCategoryByIdCache :: (CacheFlow m r) => Id IssueCategory -> Identifier -> m ()
clearIssueCategoryByIdCache issueCategoryId identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByIdKey issueCategoryId identifier

cacheIssueCategoryById :: (CacheFlow m r) => Id IssueCategory -> Identifier -> Maybe IssueCategory -> m ()
cacheIssueCategoryById issueCategoryId identifier issueCategory = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdKey issueCategoryId identifier) issueCategory expTime

makeIssueCategoryByIdKey :: Id IssueCategory -> Identifier -> Text
makeIssueCategoryByIdKey id identifier = show identifier <> "CachedQueries:IssueCategory:Id-" <> show id

--------- Caching logic for issue category by id -------------------

clearIssueCategoryByIdAndLanguageCache :: (CacheFlow m r) => Id IssueCategory -> Language -> Identifier -> m ()
clearIssueCategoryByIdAndLanguageCache issueCategoryId language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier

cacheIssueCategoryByIdAndLanguage :: (CacheFlow m r) => Id IssueCategory -> Language -> Identifier -> Maybe (IssueCategory, Maybe IssueTranslation) -> m ()
cacheIssueCategoryByIdAndLanguage issueCategoryId language identifier issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByIdAndLanguageKey issueCategoryId language identifier) issueCategoryTranslation expTime

makeIssueCategoryByIdAndLanguageKey :: Id IssueCategory -> Language -> Identifier -> Text
makeIssueCategoryByIdAndLanguageKey id language identifier = show identifier <> "CachedQueries:IssueCategory:Id-" <> show id <> ":Language-" <> show language
