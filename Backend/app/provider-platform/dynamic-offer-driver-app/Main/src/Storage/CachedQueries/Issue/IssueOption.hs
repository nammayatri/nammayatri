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

module Storage.CachedQueries.Issue.IssueOption where

import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.External.Types (Language)
import Storage.CachedQueries.CacheConfig
import qualified Storage.Queries.Issue.IssueOption as Queries

findAllByCategoryAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage issueCategoryId language =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueOptionByLanguageKey issueCategoryId language) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueOption issueCategoryId language /=<< Queries.findAllByCategoryAndLanguage issueCategoryId language

--------- Caching logic -------------------

clearIssueOptionCache :: (CacheFlow m r) => Id IssueCategory -> Language -> m ()
clearIssueOptionCache issueCategoryId = Hedis.withCrossAppRedis . Hedis.del . makeIssueOptionByLanguageKey issueCategoryId

cacheIssueOption :: (CacheFlow m r) => Id IssueCategory -> Language -> [(IssueOption, Maybe IssueTranslation)] -> m ()
cacheIssueOption issueCategoryId language issueOptionTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueOptionByLanguageKey issueCategoryId language) issueOptionTranslation expTime

makeIssueOptionByLanguageKey :: Id IssueCategory -> Language -> Text
makeIssueOptionByLanguageKey issueCategoryId language = "driver-offer:CachedQueries:IssueOption:IssueCategoryId-" <> getId issueCategoryId <> ":Language-" <> show language
