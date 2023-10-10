{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import IssueManagement.Domain.Types.Issue.IssueCategory
import IssueManagement.Domain.Types.Issue.IssueMessage
import IssueManagement.Domain.Types.Issue.IssueOption as DomainIO
import IssueManagement.Domain.Types.Issue.IssueTranslation
import qualified IssueManagement.Storage.Beam.Issue.IssueOption as BeamIO
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow (BeamFlow)
import qualified IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import qualified Sequelize as Se

findByIdAndCategoryId :: BeamFlow m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = findOneWithKV [Se.And [Se.Is BeamIO.id $ Se.Eq $ getId issueOptionId, Se.Is BeamIO.issueCategoryId $ Se.Eq $ Just (getId issueCategoryId)]]

findAllIssueOptionWithSeCondition :: BeamFlow m => [Se.Clause Postgres BeamIO.IssueOptionT] -> Se.OrderBy BeamIO.IssueOptionT -> Maybe Int -> Maybe Int -> m [IssueOption]
findAllIssueOptionWithSeCondition = findAllWithOptionsKV

findAllIssueTranslationWithSeCondition :: BeamFlow m => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllByCategoryAndLanguage :: BeamFlow m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage (Id issueCategoryId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Se.Is BeamIO.issueCategoryId $ Se.Eq (Just issueCategoryId)] (Se.Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findAllByMessageAndLanguage :: BeamFlow m => Id IssueMessage -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByMessageAndLanguage (Id issueMessageId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Se.Is BeamIO.issueMessageId $ Se.Eq $ Just issueMessageId] (Se.Asc BeamIO.priority) Nothing Nothing
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findByIdAndLanguage :: BeamFlow m => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = do
  iOptions <- findAllWithKV [Se.Is BeamIO.id $ Se.Eq (getId issueOptionId)]
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  let dInfosWithTranslations' = headMaybe $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  pure dInfosWithTranslations'
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findById :: BeamFlow m => Id IssueOption -> m (Maybe IssueOption)
findById (Id issueOptionId) = findOneWithKV [Se.Is BeamIO.id $ Se.Eq issueOptionId]

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
