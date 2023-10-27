{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.Comment where

import qualified Data.Time.LocalTime as T
import IssueManagement.Domain.Types.Issue.Comment as Comment
import IssueManagement.Domain.Types.Issue.IssueReport (IssueReport)
import qualified IssueManagement.Storage.Beam.Issue.Comment as BeamC
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => Comment.Comment -> m ()
create = createWithKV

findById :: BeamFlow m r => Id Comment -> m (Maybe Comment)
findById (Id id) = findOneWithKV [Is BeamC.id $ Eq id]

findAllByIssueReportId :: BeamFlow m r => Id IssueReport -> m [Comment]
findAllByIssueReportId (Id issueReportId) = findAllWithOptionsKV [Is BeamC.issueReportId $ Eq issueReportId] (Desc BeamC.createdAt) Nothing Nothing

instance FromTType' BeamC.Comment Comment where
  fromTType' BeamC.CommentT {..} = do
    pure $
      Just
        Comment
          { id = Id id,
            issueReportId = Id issueReportId,
            authorId = Id authorId,
            comment = comment,
            createdAt = T.localTimeToUTC T.utc createdAt
          }

instance ToTType' BeamC.Comment Comment where
  toTType' Comment {..} = do
    BeamC.CommentT
      { BeamC.id = getId id,
        BeamC.issueReportId = getId issueReportId,
        BeamC.authorId = getId authorId,
        BeamC.comment = comment,
        BeamC.createdAt = T.utcToLocalTime T.utc createdAt
      }
