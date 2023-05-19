module Storage.Queries.Issue.IssueOption where

import Domain.Types.Issue.IssueCategory
import Domain.Types.Issue.IssueOption
import Domain.Types.Issue.IssueTranslation
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.IssueOption as BeamIO
import Storage.Tabular.Issue.IssueOption
import Storage.Tabular.Issue.IssueTranslation

findByIdAndCategoryId :: Transactionable m => Id IssueOption -> Id IssueCategory -> m (Maybe IssueOption)
findByIdAndCategoryId issueOptionId issueCategoryId = Esq.findOne $ do
  issueOption <- from $ table @IssueOptionT
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
      &&. issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
  pure issueOption

fullOptionTable ::
  Language ->
  From
    ( Table IssueOptionT
        :& MbTable IssueTranslationT
    )
fullOptionTable language =
  table @IssueOptionT
    `leftJoin` table @IssueTranslationT
      `Esq.on` ( \(option :& translation) ->
                   just (option ^. IssueOptionOption) ==. translation ?. IssueTranslationSentence
                     &&. translation ?. IssueTranslationLanguage ==. just (val language)
               )

findAllByCategoryAndLanguage :: Transactionable m => Id IssueCategory -> Language -> m [(IssueOption, Maybe IssueTranslation)]
findAllByCategoryAndLanguage issueCategoryId language = Esq.findAll $ do
  (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
  where_ $
    issueOption ^. IssueOptionIssueCategoryId ==. val (toKey issueCategoryId)
  pure (issueOption, mbIssueTranslation)

findByIdAndLanguage :: Transactionable m => Id IssueOption -> Language -> m (Maybe (IssueOption, Maybe IssueTranslation))
findByIdAndLanguage issueOptionId language = Esq.findOne $ do
  (issueOption :& mbIssueTranslation) <- from $ fullOptionTable language
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
  pure (issueOption, mbIssueTranslation)

findById :: Transactionable m => Id IssueOption -> m (Maybe IssueOption)
findById issueOptionId = Esq.findOne $ do
  issueOption <- from $ table @IssueOptionT
  where_ $
    issueOption ^. IssueOptionTId ==. val (toKey issueOptionId)
  pure issueOption

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
