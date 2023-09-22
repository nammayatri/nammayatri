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

module IssueManagement.Storage.CachedQueries.Issue.IssueMessage where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueTranslation
import IssueManagement.Storage.CachedQueries.CacheConfig
import qualified IssueManagement.Storage.Queries.Issue.IssueMessage as Queries
import Kernel.External.Types (Language)
import Kernel.Prelude
import qualified Kernel.Storage.Esqueleto as Esq
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id

findById :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueMessage -> Identifier -> m (Maybe IssueMessage)
findById issueMessageId identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueMessageById issueMessageId identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueMessageById issueMessageId identifier /=<< Queries.findById issueMessageId

findByIdAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueMessage -> Language -> Identifier -> m (Maybe (IssueMessage, Maybe IssueTranslation))
findByIdAndLanguage issueMessageId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueMessageByIdAndLanguage issueMessageId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheIssueMessageByIdAndLanguage issueMessageId language identifier /=<< Queries.findByIdAndLanguage issueMessageId language

findAllByCategoryIdAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueCategory -> Language -> Identifier -> m [(IssueMessage, Maybe IssueTranslation)]
findAllByCategoryIdAndLanguage issueCategoryId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueMessageByLanguageAndCategory issueCategoryId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueMessageByCategoryIdAndLanguage issueCategoryId language identifier /=<< Queries.findAllByCategoryIdAndLanguage issueCategoryId language

findAllByOptionIdAndLanguage :: (CacheFlow m r, Esq.EsqDBFlow m r) => Id IssueOption -> Language -> Identifier -> m [(IssueMessage, Maybe IssueTranslation)]
findAllByOptionIdAndLanguage issueOptionId language identifier =
  Hedis.withCrossAppRedis (Hedis.safeGet $ makeIssueMessageByLanguageAndOption issueOptionId language identifier) >>= \case
    Just a -> pure a
    Nothing -> cacheAllIssueMessageByOptionIdAndLanguage issueOptionId language identifier /=<< Queries.findAllByOptionIdAndLanguage issueOptionId language

-- --------- Caching logic for issue message by id -------------------

clearIssueMessageByIdCache :: CacheFlow m r => Id IssueMessage -> Identifier -> m ()
clearIssueMessageByIdCache issueMessageId identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueMessageById issueMessageId identifier

cacheIssueMessageById :: CacheFlow m r => Id IssueMessage -> Identifier -> Maybe IssueMessage -> m ()
cacheIssueMessageById issueMessageId identifier issueMessage = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueMessageById issueMessageId identifier) issueMessage expTime

makeIssueMessageById :: Id IssueMessage -> Identifier -> Text
makeIssueMessageById issueMessageId identifier = show identifier <> "CachedQueries:IssueMessage:Id" <> show issueMessageId

-- --------- Caching logic for issue message by id & language -------------------

clearIssueMessageByIdAndLanguageCache :: CacheFlow m r => Id IssueMessage -> Language -> Identifier -> m ()
clearIssueMessageByIdAndLanguageCache issueMessageId language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueMessageByIdAndLanguage issueMessageId language identifier

cacheIssueMessageByIdAndLanguage :: CacheFlow m r => Id IssueMessage -> Language -> Identifier -> Maybe (IssueMessage, Maybe IssueTranslation) -> m ()
cacheIssueMessageByIdAndLanguage issueMessageId language identifier issueMessageTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueMessageByIdAndLanguage issueMessageId language identifier) issueMessageTranslation expTime

makeIssueMessageByIdAndLanguage :: Id IssueMessage -> Language -> Identifier -> Text
makeIssueMessageByIdAndLanguage issueMessageId language identifier = show identifier <> "CachedQueries:IssueMessage:CategoryId:Language-" <> show issueMessageId <> "-" <> show language

-- --------- Caching logic for issue message by issueCategoryId & language -------------------

clearIssueMessageByCategoryIdAndLanguageCache :: CacheFlow m r => Id IssueCategory -> Language -> Identifier -> m ()
clearIssueMessageByCategoryIdAndLanguageCache issueCategoryId language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueMessageByLanguageAndCategory issueCategoryId language identifier

cacheAllIssueMessageByCategoryIdAndLanguage :: CacheFlow m r => Id IssueCategory -> Language -> Identifier -> [(IssueMessage, Maybe IssueTranslation)] -> m ()
cacheAllIssueMessageByCategoryIdAndLanguage issueCategoryId language identifier issueMessageTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueMessageByLanguageAndCategory issueCategoryId language identifier) issueMessageTranslation expTime

makeIssueMessageByLanguageAndCategory :: Id IssueCategory -> Language -> Identifier -> Text
makeIssueMessageByLanguageAndCategory issueCategoryId language identifier = show identifier <> "CachedQueries:IssueMessage:CategoryId:Language-" <> show issueCategoryId <> "-" <> show language

-- --------- Caching logic for issue message by issueOptionId & language -------------------

clearIssueMessageByOptionIdAndLanguageCache :: CacheFlow m r => Id IssueOption -> Language -> Identifier -> m ()
clearIssueMessageByOptionIdAndLanguageCache issueOptionId language identifier = Hedis.withCrossAppRedis . Hedis.del $ makeIssueMessageByLanguageAndOption issueOptionId language identifier

cacheAllIssueMessageByOptionIdAndLanguage :: CacheFlow m r => Id IssueOption -> Language -> Identifier -> [(IssueMessage, Maybe IssueTranslation)] -> m ()
cacheAllIssueMessageByOptionIdAndLanguage issueOptionId language identifier issueMessageTranslation = do
  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
  Hedis.withCrossAppRedis $ Hedis.setExp (makeIssueMessageByLanguageAndOption issueOptionId language identifier) issueMessageTranslation expTime

makeIssueMessageByLanguageAndOption :: Id IssueOption -> Language -> Identifier -> Text
makeIssueMessageByLanguageAndOption issueOptionId language identifier = show identifier <> "CachedQueries:IssueMessage:OptionId:Language-" <> show issueOptionId <> "-" <> show language
