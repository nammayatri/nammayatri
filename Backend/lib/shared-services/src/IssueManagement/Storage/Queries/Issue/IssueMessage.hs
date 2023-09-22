{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueMessage where

import Database.Beam.Postgres (Postgres)
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage as DomainIM
import IssueManagement.Domain.Types.Issue.IssueOption
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Storage.Beam.Issue.IssueMessage as BeamIM
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH hiding (label)
import Kernel.External.Types (Language)
import Kernel.Types.Id

findAllIssueTranslationWithSeCondition :: BeamFlow m => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueMessageWithSeCondition :: BeamFlow m => [Clause Postgres BeamIM.IssueMessageT] -> OrderBy BeamIM.IssueMessageT -> Maybe Int -> Maybe Int -> m [IssueMessage]
findAllIssueMessageWithSeCondition = findAllWithOptionsKV

findById :: BeamFlow m => Id IssueMessage -> m (Maybe IssueMessage)
findById issueMessageId = findOneWithKV [Is BeamIM.id $ Eq (getId issueMessageId)]

findByIdAndLanguage :: BeamFlow m => Id IssueMessage -> Language -> m (Maybe (IssueMessage, Maybe IssueTranslation))
findByIdAndLanguage issueMessageId language = do
  iMessages <- findAllWithKV [Is BeamIM.id $ Eq (getId issueMessageId)]
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIM.message <$> iMessages)]]
  pure $ headMaybe $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iMessage.message) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iMessage, Just iTranslation'')) <$> iTranslations' else [(iMessage, Nothing)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findAllByCategoryIdAndLanguage :: BeamFlow m => Id IssueCategory -> Language -> m [(IssueMessage, Maybe IssueTranslation)]
findAllByCategoryIdAndLanguage (Id issueCategoryId) language = do
  iMessages <- findAllIssueMessageWithSeCondition [Is BeamIM.categoryId $ Eq $ Just issueCategoryId] (Asc BeamIM.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIM.message <$> iMessages)]]
  pure $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iMessage.message) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iMessage, Just iTranslation'')) <$> iTranslations' else [(iMessage, Nothing)]

findAllByOptionIdAndLanguage :: BeamFlow m => Id IssueOption -> Language -> m [(IssueMessage, Maybe IssueTranslation)]
findAllByOptionIdAndLanguage (Id issueOptionId) language = do
  iMessages <- findAllIssueMessageWithSeCondition [Is BeamIM.optionId $ Eq $ Just issueOptionId] (Asc BeamIM.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIM.message <$> iMessages)]]
  pure $ foldl' (getIssueMessagesWithTranslations iTranslations) [] iMessages
  where
    getIssueMessagesWithTranslations iTranslations dInfosWithTranslations iMessage =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iMessage.message) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iMessage, Just iTranslation'')) <$> iTranslations' else [(iMessage, Nothing)]

instance FromTType' BeamIM.IssueMessage IssueMessage where
  fromTType' BeamIM.IssueMessageT {..} = do
    pure $
      Just
        IssueMessage
          { id = Id id,
            categoryId = Id <$> categoryId,
            optionId = Id <$> optionId,
            message = message,
            priority = priority,
            label = label
          }

instance ToTType' BeamIM.IssueMessage IssueMessage where
  toTType' IssueMessage {..} = do
    BeamIM.IssueMessageT
      { BeamIM.id = getId id,
        BeamIM.categoryId = getId <$> categoryId,
        BeamIM.optionId = getId <$> optionId,
        BeamIM.message = message,
        BeamIM.priority = priority,
        label = label
      }
