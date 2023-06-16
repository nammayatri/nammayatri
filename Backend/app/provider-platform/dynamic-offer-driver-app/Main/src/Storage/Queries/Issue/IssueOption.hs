module Storage.Queries.Issue.IssueOption where

import Database.Beam.Postgres (Postgres)
import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption as DomainIO
import Domain.Types.Issue.IssueTranslation
import qualified EulerHS.KVConnector.Flow as KV
import qualified EulerHS.Language as L
import qualified Kernel.Beam.Types as KBT
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Lib.Utils (setMeshConfig)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueOption as BeamIO
import qualified Storage.Beam.Issue.IssueTranslation as BeamIT
import qualified Storage.Queries.Issue.IssueTranslation as QueriesIT

-- findByIdAndCategoryId :: Transactionable m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
-- findByIdAndCategoryId issueOptionId issueCategoryId = Esq.findOne $ do
--   issueOption <- from $ table @IssueOptionT
--   where_ $
--     issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
--       &&. issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
--   pure issueOption

findByIdAndCategoryId :: L.MonadFlow m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamIO.IssueOptionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure Nothing) (transformBeamIssueOptionToDomain <$>) <$> KV.findWithKVConnector dbConf' updatedMeshConfig [Se.And [Se.Is BeamIO.id $ Se.Eq $ getId issueOptionId, Se.Is BeamIO.issueCategoryId $ Se.Eq $ getId issueCategoryId]]
    Nothing -> pure Nothing

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

findAllIssueOptionWithSeCondition :: L.MonadFlow m => [Se.Clause Postgres BeamIO.IssueOptionT] -> m [IssueOption]
findAllIssueOptionWithSeCondition seCondition = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamIO.IssueOptionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (transformBeamIssueOptionToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig seCondition
    Nothing -> pure []

findAllIssueTranslationWithSeCondition :: L.MonadFlow m => [Se.Clause Postgres BeamIT.IssueTranslationT] -> m [IssueTranslation]
findAllIssueTranslationWithSeCondition seCondition = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamIT.IssueTranslationT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> either (pure []) (QueriesIT.transformBeamIssueTranslationToDomain <$>) <$> KV.findAllWithKVConnector dbConf' updatedMeshConfig seCondition
    Nothing -> pure []

findAllByCategoryAndLanguage :: L.MonadFlow m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
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

findByIdAndLanguage :: L.MonadFlow m => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
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

findById :: L.MonadFlow m => Id IssueOption -> m (Maybe IssueOption)
findById (Id issueOptionId) = do
  dbConf <- L.getOption KBT.PsqlDbCfg
  let modelName = Se.modelTableName @BeamIO.IssueOptionT
  let updatedMeshConfig = setMeshConfig modelName
  case dbConf of
    Just dbConf' -> do
      result <- KV.findWithKVConnector dbConf' updatedMeshConfig [Se.Is BeamIO.id $ Se.Eq issueOptionId]
      case result of
        Right issueOption -> pure $ transformBeamIssueOptionToDomain <$> issueOption
        Left _ -> pure Nothing
    Nothing -> pure Nothing

transformBeamIssueOptionToDomain :: BeamIO.IssueOption -> IssueOption
transformBeamIssueOptionToDomain BeamIO.IssueOptionT {..} = do
  IssueOption
    { id = Id id,
      issueCategoryId = Id issueCategoryId,
      option = option
    }

transformDomainIssueOptionToBeam :: IssueOption -> BeamIO.IssueOption
transformDomainIssueOptionToBeam IssueOption {..} =
  BeamIO.IssueOptionT
    { BeamIO.id = getId id,
      BeamIO.issueCategoryId = getId issueCategoryId,
      BeamIO.option = option
    }
