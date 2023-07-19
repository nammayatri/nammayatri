{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption as DomainIO
import Domain.Types.Issue.IssueTranslation
import qualified EulerHS.Language as L
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), findAllWithKV, findOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueOption as BeamIO
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified Storage.Queries.Issue.IssueTranslation ()

-- findByIdAndCategoryId :: Transactionable m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
-- findByIdAndCategoryId issueOptionId issueCategoryId = Esq.findOne $ do
--   issueOption <- from $ table @IssueOptionT
--   where_ $
--     issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
--       &&. issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
--   pure issueOption

findByIdAndCategoryId :: (L.MonadFlow m, Log m) => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = findOneWithKV [Se.And [Se.Is BeamIO.id $ Se.Eq $ getId issueOptionId, Se.Is BeamIO.issueCategoryId $ Se.Eq $ getId issueCategoryId]]

-- fullOptionTable ::
--   Language ->
--   From
--     ( Table IssueOptionT
--         :& MbTable IssueTranslationT
--     )
-- fullOptionTable language =
--   table @IssueOptionT
--     `leftJoin` table @IssueTranslationT
--       `Esq.on` ( \(option :& translation) ->
--                    just (option ^. IssueOptionOption) ==. translation ?. IssueTranslationSentence
--                      &&. translation ?. IssueTranslationLanguage ==. just (val language)
--                )

-- findAllByCategoryAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
-- findAllByCategoryAndLanguage issueCategoryId language = Esq.findAll $ do
--   (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
--   where_ $
--     issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
--   pure (issueOption, mbIssueTranslation)

findAllIssueOptionWithSeCondition :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamIO.IssueOptionT] -> m [IssueOption]
findAllIssueOptionWithSeCondition = findAllWithKV

findAllIssueTranslationWithSeCondition :: (L.MonadFlow m, Log m) => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition = findAllWithKV

findAllByCategoryAndLanguage :: (L.MonadFlow m, Log m) => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage (Id issueCategoryId) language = do
  let iOptionsSeCondition = [Se.Is BeamIO.issueCategoryId $ Se.Eq issueCategoryId]
  iOptions <- findAllIssueOptionWithSeCondition iOptionsSeCondition
  let iTranslationsSeCondition = [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  iTranslations <- findAllIssueTranslationWithSeCondition iTranslationsSeCondition
  let dInfosWithTranslations = foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
  pure dInfosWithTranslations
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]

-- getDriverInfoWithDL drLocs dInfosWithLocs dInfo =
--   let drLocs' = filter (\drLoc -> drLoc.driverId == dInfo.driverId) drLocs
--    in dInfosWithLocs <> ((\drLoc -> (dInfo, drLoc)) <$> drLocs')

-- findByIdAndLanguage :: Transactionable m => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
-- findByIdAndLanguage issueOptionId language = Esq.findOne $ do
--   (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
--   where_ $
--     issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
--   pure (issueOption, mbIssueTranslation)

findByIdAndLanguage :: (L.MonadFlow m, Log m) => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = do
  let iOptionsSeCondition = [Se.Is BeamIO.id $ Se.Eq (getId issueOptionId)]
  iOptions <- findAllIssueOptionWithSeCondition iOptionsSeCondition
  let iTranslationsSeCondition = [Se.And [Se.Is BeamIT.language $ Se.Eq language, Se.Is BeamIT.sentence $ Se.In (DomainIO.option <$> iOptions)]]
  iTranslations <- findAllIssueTranslationWithSeCondition iTranslationsSeCondition
  let dInfosWithTranslations' = foldl' (getIssueOptionsWithTranslations iTranslations) [] iOptions
      dInfosWithTranslations = headMaybe dInfosWithTranslations'
  pure dInfosWithTranslations
  where
    getIssueOptionsWithTranslations iTranslations dInfosWithTranslations iOption =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iOption.option) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then (\iTranslation'' -> (iOption, Just iTranslation'')) <$> iTranslations' else [(iOption, Nothing)]
    headMaybe dInfosWithTranslations' = if null dInfosWithTranslations' then Nothing else Just (head dInfosWithTranslations')

-- findById :: Transactionable m => Id IssueOption -> m (Maybe IssueOption)
-- findById issueOptionId = Esq.findOne $ do
--   issueOption <- from $ table @IssueOptionT
--   where_ $
--     issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
--   pure issueOption

findById :: (L.MonadFlow m, Log m) => Id IssueOption -> m (Maybe IssueOption)
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
