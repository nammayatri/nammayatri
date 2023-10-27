{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption as DomainIO
import Domain.Types.Issue.IssueTranslation
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueOption as BeamIO
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified Storage.Queries.Issue.IssueTranslation ()

findByIdAndCategoryId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = findOneWithKV [Se.And [Se.Is BeamIO.id $ Se.Eq $ getId issueOptionId, Se.Is BeamIO.issueCategoryId $ Se.Eq $ getId issueCategoryId]]

findAllIssueOptionWithSeCondition :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamIO.IssueOptionT] -> m [IssueOption]
findAllIssueOptionWithSeCondition = findAllWithKV

findAllIssueTranslationWithSeCondition :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllByCategoryAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage (Id issueCategoryId) language = do
  iOptions <- findAllIssueOptionWithSeCondition [Se.Is BeamIO.issueCategoryId $ Se.Eq issueCategoryId]
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  pure $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

findByIdAndLanguage :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = do
  iOptions <- findAllIssueOptionWithSeCondition [Se.Is BeamIO.id $ Se.Eq (getId issueOptionId)]
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  let dInfosWithTranslations' = headMaybe $ foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  pure dInfosWithTranslations'
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

findById :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id IssueOption -> m (Maybe IssueOption)
findById (Id issueOptionId) = findOneWithKV [Se.Is BeamIO.id $ Se.Eq issueOptionId]

instance FromTType' BeamIO.IssueOption IssueOption where
  fromTType' BeamIO.IssueOptionT {..} = do
    pure $
      Just
        IssueOption
          { id = Id id,
            issueCategoryId = Id issueCategoryId,
            option = option
          }

instance ToTType' BeamIO.IssueOption IssueOption where
  toTType' IssueOption {..} = do
    BeamIO.IssueOptionT
      { BeamIO.id = getId id,
        BeamIO.issueCategoryId = getId issueCategoryId,
        BeamIO.option = option
      }
