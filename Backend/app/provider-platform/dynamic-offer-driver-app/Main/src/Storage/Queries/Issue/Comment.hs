module Storage.Queries.Issue.Comment where

import Domain.Types.Issue.Comment as Comment
import Domain.Types.Issue.IssueReport (IssueReport)
import qualified EulerHS.Extra.EulerDB as Extra
import qualified EulerHS.KVConnector.Flow as KV
import EulerHS.KVConnector.Types
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import qualified Lib.Mesh as Mesh
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.Comment as BeamC
import Storage.Tabular.Issue.Comment
import qualified Storage.Tabular.VechileNew as VN

-- create :: Comment -> SqlDB ()
-- create = Esq.create

create :: L.MonadFlow m => Comment.Comment -> m (MeshResult ())
create comment = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbConf' -> KV.createWoReturingKVConnector dbConf' VN.meshConfig (transformDomainCommentToBeam comment)
    Nothing -> pure (Left $ MKeyNotFound "DB Config not found")

-- findAllByIssueReportId :: Transactionable m => Id IssueReport -> m [Comment]
-- findAllByIssueReportId issueReportId = findAll $ do
--   comment <- from $ table @CommentT
--   where_ $ comment ^. CommentIssueReportId ==. val (toKey issueReportId)
--   orderBy [desc $ comment ^. CommentCreatedAt]
--   return comment

findAllByIssueReportId :: L.MonadFlow m => Id IssueReport -> m [Comment]
findAllByIssueReportId (Id issueReportId) = do
  dbConf <- L.getOption Extra.EulerPsqlDbCfg
  case dbConf of
    Just dbCOnf' -> either (pure []) (transformBeamCommentToDomain <$>) <$> KV.findAllWithOptionsKVConnector dbCOnf' Mesh.meshConfig [Se.Is BeamC.issueReportId $ Se.Eq issueReportId] (Se.Desc BeamC.createdAt) Nothing Nothing
    Nothing -> pure []

transformBeamCommentToDomain :: BeamC.Comment -> Comment
transformBeamCommentToDomain BeamC.CommentT {..} = do
  Comment
    { id = Id id,
      issueReportId = Id issueReportId,
      authorId = Id authorId,
      comment = comment,
      createdAt = createdAt
    }

transformDomainCommentToBeam :: Comment -> BeamC.Comment
transformDomainCommentToBeam Comment {..} =
  BeamC.CommentT
    { BeamC.id = getId id,
      BeamC.issueReportId = getId issueReportId,
      BeamC.authorId = getId authorId,
      BeamC.comment = comment,
      BeamC.createdAt = createdAt
    }
