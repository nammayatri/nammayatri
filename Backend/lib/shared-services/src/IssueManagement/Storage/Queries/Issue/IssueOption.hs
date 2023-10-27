{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption as DomainIO
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH hiding (label)
import Kernel.External.Types (Language)
import Kernel.Types.Id

findByIdAndCategoryId :: BeamFlow m r => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = findOneWithKV [And [Is BeamIO.id $ Eq $ getId issueOptionId, Is BeamIO.issueCategoryId $ Eq $ Just (getId issueCategoryId)]]

findAllIssueOptionWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIO.IssueOptionT] -> OrderBy BeamIO.IssueOptionT -> Maybe Int -> Maybe Int -> m [IssueOption]
findAllIssueOptionWithSeCondition = findAllWithOptionsKV

findAllIssueTranslationWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllByCategoryAndLanguage :: BeamFlow m r => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage (Id issueCategoryId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Is BeamIO.issueCategoryId $ Eq (Just issueCategoryId)] (Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findAllByMessageAndLanguage :: BeamFlow m r => Id IssueMessage -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByMessageAndLanguage (Id issueMessageId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Is BeamIO.issueMessageId $ Eq $ Just issueMessageId] (Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findByIdAndLanguage :: BeamFlow m r => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = do
  iOptions <- findAllWithKV [Is BeamIO.id $ Eq (getId issueOptionId)]
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIO.option <$> iOptions)]]
  let dInfosWithTranslations' = headMaybe $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  pure dInfosWithTranslations'
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findById :: BeamFlow m r => Id IssueOption -> m (Maybe IssueOption)
findById (Id issueOptionId) = findOneWithKV [Is BeamIO.id $ Eq issueOptionId]

instance FromTType' BeamIO.IssueOption IssueOption where
  fromTType' BeamIO.IssueOptionT {..} = do
    pure $
      Just
        IssueOption
          { id = Id id,
            issueCategoryId = Id <$> issueCategoryId,
            option = option,
            priority = priority,
            issueMessageId = issueMessageId,
            label = label
          }

instance ToTType' BeamIO.IssueOption IssueOption where
  toTType' IssueOption {..} = do
    BeamIO.IssueOptionT
      { BeamIO.id = getId id,
        BeamIO.issueCategoryId = getId <$> issueCategoryId,
        BeamIO.option = option,
        BeamIO.priority = priority,
        BeamIO.issueMessageId = issueMessageId,
        BeamIO.label = label
      }
