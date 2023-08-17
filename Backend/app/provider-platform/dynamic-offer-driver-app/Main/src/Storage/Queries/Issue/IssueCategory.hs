{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Storage.Queries.Issue.IssueCategory where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Issue.IssueCategory
import qualified Domain.Types.Issue.IssueCategory as DomainIC
import Domain.Types.Issue.IssueTranslation as DomainIT
import qualified EulerHS.Language as L
import Kernel.Beam.Functions
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueCategory as BeamIC
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import Storage.Queries.Issue.IssueTranslation ()

-- fullCategoryTable ::
--   Language ->
--   From
--     ( Table IssueCategoryT
--         :& MbTable IssueTranslationT
--     )
-- fullCategoryTable language =
--   table @IssueCategoryT
--     `leftJoin` table @IssueTranslationT
--       `Esq.on` ( \(category :& translation) ->
--                    just (category ^. IssueCategoryCategory) ==. translation ?. IssueTranslationSentence
--                      &&. translation ?. IssueTranslationLanguage ==. just (val language)
--                )

-- findAllByLanguage :: Transactionable m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
-- findAllByLanguage language = Esq.findAll $ do
--   (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
--   return (issueCategory, mbIssueTranslation)

findAllIssueTranslationWithSeCondition :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllIssueCategoryWithSeCondition :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamIC.IssueCategoryT] -> m [IssueCategory]
findAllIssueCategoryWithSeCondition = findAllWithKV

findAllByLanguage :: (L.MonadFlow m, Log m) => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language = do
  iTranslations <- findAllIssueTranslationWithSeCondition [Se.Is BeamIT.language $ Se.Eq language]
  iCategorys <- findAllIssueCategoryWithSeCondition [Se.Is BeamIC.category $ Se.In (DomainIT.sentence <$> iTranslations)]
  pure $ foldl' (getIssueCategoryWithTranslations iTranslations) [] iCategorys
  where
    getIssueCategoryWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations' else [(iCategory, Nothing)]

-- findById :: Transactionable m => Id IssueCategory -> m (Maybe IssueCategory)
-- findById issueCategoryId = Esq.findOne $ do
--   issueCategory <- from $ table @IssueCategoryT
--   where_ $ issueCategory ^. IssueCategoryTId ==. val (toKey issueCategoryId)
--   return issueCategory

findById :: (L.MonadFlow m, Log m) => Id IssueCategory -> m (Maybe IssueCategory)
findById (Id issueCategoryId) = findOneWithKV [Se.Is BeamIC.id $ Se.Eq issueCategoryId]

-- findByIdAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
-- findByIdAndLanguage issueCategoryId language = Esq.findOne $ do
--   (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
--   return (issueCategory, mbIssueTranslation)

findByIdAndLanguage :: (L.MonadFlow m, Log m) => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
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
