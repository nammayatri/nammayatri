{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.IssueCategory where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Issue.IssueCategory
import qualified Domain.Types.Issue.IssueCategory as DomainIC
import Domain.Types.Issue.IssueTranslation as DomainIT
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueCategory as BeamIC
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import Storage.Queries.Issue.IssueTranslation ()

findAllIssueTranslationWithSeCondition :: MonadFlow m => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueCategoryWithSeCondition :: MonadFlow m => [Se.Clause Postgres BeamIC.IssueCategoryT] -> m [IssueCategory]
findAllIssueCategoryWithSeCondition = findAllWithKV

findAllByLanguage :: MonadFlow m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language = do
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.Is BeamIT.language $ Se.Eq language]
  iCategorys <- findAllIssueCategoryWithSeCondition [Se.Is BeamIC.category $ Se.In (DomainIT.sentence <$> iTranslations)]
  pure $ foldl' (getIssueCategoryWithTranslations iTranslations) [] iCategorys
  where
    getIssueCategoryWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

findById :: MonadFlow m => Id IssueCategory -> m (Maybe IssueCategory)
findById (Id issueCategoryId) = findOneWithKV [Se.Is BeamIC.id $ Se.Eq issueCategoryId]

findByIdAndLanguage :: MonadFlow m => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage (Id issueCategoryId) language = do
  iCategory <- findAllIssueCategoryWithSeCondition [Se.Is BeamIC.id $ Se.Eq issueCategoryId]
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIC.category <$> iCategory)]]
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
