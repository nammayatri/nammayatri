{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue.Comment where

import Data.Time
import qualified Domain.Types.Issue.Comment as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Issue.IssueReport (IssueReportTId)

mkPersist
  defaultSqlSettings
  [defaultQQ|
    CommentT sql=comment
      id Text
      issueReportId IssueReportTId
      author Text
      comment Text
      createdAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey CommentT where
  type DomainKey CommentT = Id Domain.Comment
  fromKey (CommentTKey _id) = Id _id
  toKey (Id id) = CommentTKey id

instance FromTType CommentT Domain.Comment where
  fromTType CommentT {..} = do
    return $
      Domain.Comment
        { id = Id id,
          issueReportId = fromKey issueReportId,
          ..
        }

instance ToTType CommentT Domain.Comment where
  toTType Domain.Comment {..} =
    CommentT
      { id = getId id,
        issueReportId = toKey issueReportId,
        ..
      }
