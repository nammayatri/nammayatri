{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Issue.Comment where

import Domain.Types.Issue.Comment as Comment
import Domain.Types.Issue.IssueReport (IssueReport)
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Types.Logging (Log)
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithOptionsKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Issue.Comment as BeamC

-- create :: Comment -> SqlDB ()
-- create = Esq.create

create :: (L.MonadFlow m, Log m) => Comment.Comment -> m ()
create = createWithKV

-- findAllByIssueReportId :: Transactionable m => Id IssueReport -> m [Comment]
-- findAllByIssueReportId issueReportId = findAll $ do
--   comment <- from $ table @CommentT
--   where_ $ comment ^. CommentIssueReportId ==. val (toKey issueReportId)
--   orderBy [desc $ comment ^. CommentCreatedAt]
--   return comment

findAllByIssueReportId :: (L.MonadFlow m, Log m) => Id IssueReport -> m [Comment]
findAllByIssueReportId (Id issueReportId) = findAllWithOptionsKV [Se.Is BeamC.issueReportId $ Se.Eq issueReportId] (Se.Desc BeamC.createdAt) Nothing Nothing

instance FromTType' BeamC.Comment Comment where
  fromTType' BeamC.CommentT {..} = do
    pure $
      Just
        Comment
          { id = Id id,
            issueReportId = Id issueReportId,
            authorId = Id authorId,
            comment = comment,
            createdAt = createdAt
          }

instance ToTType' BeamC.Comment Comment where
  toTType' Comment {..} = do
    BeamC.CommentT
      { BeamC.id = getId id,
        BeamC.issueReportId = getId issueReportId,
        BeamC.authorId = getId authorId,
        BeamC.comment = comment,
        BeamC.createdAt = createdAt
      }
