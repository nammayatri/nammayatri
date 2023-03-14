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

module Storage.CachedQueries.Issue.IssueCategory where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.External.Types (Language)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Issue.IssueCategory as Queries

findByLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findByLanguage language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueCategoryByLanguageKey language) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueCategory language /=<< Queries.findByLanguage language

--------- Caching logic -------------------

clearIssueCategoryCache :: (CacheFlow m r) => Language -> m ()
clearIssueCategoryCache = Hedis.withCrossAppRedis . Hedis.del . makeIssueCategoryByLanguageKey

cacheIssueCategory :: (CacheFlow m r) => Language -> [(IssueCategory, Maybe IssueTranslation)] -> m ()
cacheIssueCategory language issueCategoryTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueCategoryByLanguageKey language) issueCategoryTranslation expTime

makeIssueCategoryByLanguageKey :: Language -> Text
makeIssueCategoryByLanguageKey language = "driver-offer:CachedQueries:IssueCategory:Language-" <> show language
