{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IGMIssueAction where

import IssueManagement.Domain.Types.Issue.IGMIssue (IGMIssue)
import IssueManagement.Domain.Types.Issue.IGMIssueAction as Domain
import qualified IssueManagement.Storage.Beam.Issue.IGMIssueAction as Beam
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => Domain.IGMIssueAction -> m ()
create = createWithKV

createMany :: BeamFlow m r => [Domain.IGMIssueAction] -> m ()
createMany = traverse_ createWithKV

findAllByIgmIssueId :: BeamFlow m r => Id IGMIssue -> m [IGMIssueAction]
findAllByIgmIssueId (Id igmIssueId) =
  findAllWithOptionsKV [Is Beam.igmIssueId $ Eq igmIssueId] (Asc Beam.updatedAt) Nothing Nothing

findAllByIgmIssueIdAndType :: BeamFlow m r => Id IGMIssue -> ActionType -> m [IGMIssueAction]
findAllByIgmIssueIdAndType (Id igmIssueId) aType =
  findAllWithOptionsKV
    [Is Beam.igmIssueId $ Eq igmIssueId, Is Beam.actionType $ Eq aType]
    (Asc Beam.updatedAt)
    Nothing
    Nothing

instance FromTType' Beam.IGMIssueAction Domain.IGMIssueAction where
  fromTType' Beam.IGMIssueActionT {..} = do
    pure $
      Just
        Domain.IGMIssueAction
          { id = Id id,
            igmIssueId = Id igmIssueId,
            ..
          }

instance ToTType' Beam.IGMIssueAction Domain.IGMIssueAction where
  toTType' Domain.IGMIssueAction {..} =
    Beam.IGMIssueActionT
      { Beam.id = getId id,
        Beam.igmIssueId = getId igmIssueId,
        Beam.actionType = actionType,
        Beam.action = action,
        Beam.shortDesc = shortDesc,
        Beam.updatedAt = updatedAt,
        Beam.updatedByOrgName = updatedByOrgName,
        Beam.updatedByContactPhone = updatedByContactPhone,
        Beam.updatedByContactEmail = updatedByContactEmail,
        Beam.updatedByPersonName = updatedByPersonName,
        Beam.cascadedLevel = cascadedLevel,
        Beam.createdAt = createdAt
      }
