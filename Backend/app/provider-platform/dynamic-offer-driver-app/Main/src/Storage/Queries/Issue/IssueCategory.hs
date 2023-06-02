module Storage.Queries.Issue.IssueCategory where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueTranslation as DomainIT
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueCategory as BeamIC
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified Storage.Queries.Issue.IssueTranslation as QueriesIT
import Storage.Tabular.Issue.IssueCategory
import Storage.Tabular.Issue.IssueTranslation

fullCategoryTable ::
  Language ->
  From
    ( Table IssueCategoryT
        :& MbTable IssueTranslationT
    )
fullCategoryTable language =
  table @IssueCategoryT
    `leftJoin` table @IssueTranslationT
      `Esq.on` ( \(category :& translation) ->
                   just (category ^. IssueCategoryCategory) ==. translation ?. IssueTranslationSentence
                     &&. translation ?. IssueTranslationLanguage ==. just (val language)
               )

-- findAllByLanguage :: Transactionable m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
-- findAllByLanguage language = Esq.findAll $ do
--   (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
--   return (issueCategory, mbIssueTranslation)

findAllByLanguage :: L.MonadFlow m => Language -> m [(IssueCategory, Maybe IssueTranslation)]
findAllByLanguage language = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> do
      iTranslations <- either (pure []) (QueriesIT.transformBeamIssueTranslationToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIT.language $ Se.Eq language]
      iCategorys <- either (pure []) (transformBeamIssueCategoryToDomain <$>) <$> KV.findAllWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIC.category $ Se.In (DomainIT.sentence <$> iTranslations)]
      let dCategoriesWithTranslations = foldl' (getIssueCategoryWithTranslations iTranslations) [] iCategorys
      pure dCategoriesWithTranslations
    Nothing -> pure []
  where
    getIssueCategoryWithTranslations iTranslations dInfosWithTranslations iCategory =
      let iTranslations' = filter (\iTranslation -> iTranslation.sentence == iCategory.category) iTranslations
       in dInfosWithTranslations <> if not (null iTranslations') then ((\iTranslation'' -> (iCategory, Just iTranslation'')) <$> iTranslations') else [(iCategory, Nothing)]

-- findById :: Transactionable m => Id IssueCategory -> m (Maybe IssueCategory)
-- findById issueCategoryId = Esq.findOne $ do
--   issueCategory <- from $ table @IssueCategoryT
--   where_ $ issueCategory ^. IssueCategoryTId ==. val (toKey issueCategoryId)
--   return issueCategory

findById :: L.MonadFlow m => Id IssueCategory -> m (Maybe IssueCategory)
findById (Id issueCategoryId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure Nothing) (transformBeamIssueCategoryToDomain <$>) <$> KV.findWithKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamIC.id $ Se.Eq issueCategoryId]
    Nothing -> pure Nothing

findByIdAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m (Maybe (IssueCategory, Maybe IssueTranslation))
findByIdAndLanguage issueCategoryId language = Esq.findOne $ do
  (issueCategory :& mbIssueTranslation) <- from $ fullCategoryTable language
  where_ $ issueCategory ^. IssueCategoryTId ==. val (toKey issueCategoryId)
  return (issueCategory, mbIssueTranslation)

transformBeamIssueCategoryToDomain :: BeamIC.IssueCategory -> IssueCategory
transformBeamIssueCategoryToDomain BeamIC.IssueCategoryT {..} = do
  IssueCategory
    { id = Id id,
      category = category,
      logoUrl = logoUrl
    }

transformDomainIssueCategoryToBeam :: IssueCategory -> BeamIC.IssueCategory
transformDomainIssueCategoryToBeam IssueCategory {..} =
  BeamIC.IssueCategoryT
    { BeamIC.id = getId id,
      BeamIC.category = category,
      BeamIC.logoUrl = logoUrl
    }
