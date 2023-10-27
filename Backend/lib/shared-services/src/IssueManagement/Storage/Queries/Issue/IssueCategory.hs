{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueCategory where

import Database.Beam.Postgres (Postgres)
import IssueManagement.Domain.Types.Issue.IssueCategory
import qualified IssueManagement.Domain.Types.Issue.IssueCategory as DomainIC
import IssueManagement.Domain.Types.Issue.IssueTranslation as DomainIT
import qualified IssueManagement.Storage.Beam.Issue.IssueCategory as BeamIC
import qualified IssueManagement.Storage.Beam.Issue.IssueTranslation as BeamIT
import IssueManagement.Storage.BeamFlow
import IssueManagement.Storage.Queries.Issue.IssueTranslation ()
import IssueManagement.Tools.UtilsTH
import Kernel.External.Types (Language)
import Kernel.Types.Id

findAllIssueTranslationWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueCategoryWithSeCondition :: BeamFlow m r => [Clause Postgres BeamIC.IssueCategoryT] -> m [IssueCategory]
findAllIssueCategoryWithSeCondition = findAllWithKV

findAllByLanguage :: BeamFlow m r => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language = do
  iTranslations <- findAllIssueTranslationWithSeCondition [Is BeamIT.language $ Eq language]
  iCategorys <- findAllIssueCategoryWithSeCondition [Is BeamIC.category $ In (DomainIT.sentence <$> iTranslations)]
  pure $ foldl' (getIssueCategoryWithTranslations iTranslations) [] iCategorys
  where
    getIssueCategoryWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

findById :: BeamFlow m r => Id IssueCategory -> m (Maybe IssueCategory)
findById (Id issueCategoryId) = findOneWithKV [Is BeamIC.id $ Eq issueCategoryId]

findByIdAndLanguage :: BeamFlow m r => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage (Id issueCategoryId) language = do
  iCategory <- findAllIssueCategoryWithSeCondition [Is BeamIC.id $ Eq issueCategoryId]
  iTranslations <- findAllIssueTranslationWithSeCondition [And [Is BeamIT.language $ Eq language, Is BeamIT.sentence $ In (DomainIC.category <$> iCategory)]]
  let dInfosWithTranslations' = foldl' (getIssueOptionsWithTranslations iTranslations) [] iCategory
      dInfosWithTranslations = headMaybe dInfosWithTranslations'
  pure dInfosWithTranslations
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

instance FromTType' BeamIC.IssueCategory IssueCategory where
  fromTType' BeamIC.IssueCategoryT {..} = do
    pure $
      Just
        IssueCategory
          { id = Id id,
            category = category,
            logoUrl = logoUrl
          }

instance ToTType' BeamIC.IssueCategory IssueCategory where
  toTType' IssueCategory {..} = do
    BeamIC.IssueCategoryT
      { BeamIC.id = getId id,
        BeamIC.category = category,
        BeamIC.logoUrl = logoUrl
      }
