{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Tabular.Issue.IssueReport where

import qualified Domain.Types.Issue.IssueReport as Domain
import Kernel.Prelude
import Kernel.Storage.Esqueleto
import Kernel.Types.Id
import Storage.Tabular.Message.MediaFile (MediaFileTId)
import Storage.Tabular.Person (PersonTId)
import Storage.Tabular.Ride (RideTId)

derivePersistField "Domain.IssueStatus"

mkPersist
  defaultSqlSettings
  [defaultQQ|
    IssueReportT sql=issue_report
      id Text
      driverId PersonTId
      rideId RideTId Maybe
      description Text
      assignee Text Maybe
      status Domain.IssueStatus
      category Text
      option Text Maybe
      deleted Bool
      mediaFiles (PostgresList MediaFileTId)
      createdAt UTCTime
      updatedAt UTCTime
      Primary id
      deriving Generic
    |]

instance TEntityKey IssueReportT where
  type DomainKey IssueReportT = Id Domain.IssueReport
  fromKey (IssueReportTKey _id) = Id _id
  toKey (Id id) = IssueReportTKey id

instance TType IssueReportT Domain.IssueReport where
  fromTType IssueReportT {..} = do
    return $
      Domain.IssueReport
        { id = Id id,
          mediaFiles = map fromKey (unPostgresList mediaFiles),
          driverId = fromKey driverId,
          rideId = fromKey <$> rideId,
          ..
        }
  toTType Domain.IssueReport {..} =
    IssueReportT
      { id = getId id,
        mediaFiles = PostgresList (map toKey mediaFiles),
        driverId = toKey driverId,
        rideId = toKey <$> rideId,
        ..
      }
