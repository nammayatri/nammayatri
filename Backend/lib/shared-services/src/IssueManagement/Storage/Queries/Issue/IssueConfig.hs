{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module IssueManagement.Storage.Queries.Issue.IssueConfig where

import IssueManagement.Domain.Types.Issue.IssueConfig
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIC
import IssueManagement.Storage.BeamFlow (BeamFlow)
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Sequelize as Se

findOne :: BeamFlow m => m (Maybe IssueConfig)
findOne = findOneWithKV [Se.Is BeamIC.id $ Se.Not $ Se.Eq ""]

instance FromTType' BeamIC.IssueConfig IssueConfig where
  fromTType' BeamIC.IssueConfigT {..} = do
    pure $
      Just
        IssueConfig
          { id = Id id,
            onCreateIssueMsgs = Id <$> onCreateIssueMsgs,
            onAutoMarkIssueResMsgs = Id <$> onAutoMarkIssueResMsgs,
            onIssueReopenMsgs = Id <$> onIssueReopenMsgs,
            onKaptMarkIssueAwtMsgs = Id <$> onKaptMarkIssueAwtMsgs,
            ..
          }

instance ToTType' BeamIC.IssueConfig IssueConfig where
  toTType' IssueConfig {..} = do
    BeamIC.IssueConfigT
      { BeamIC.id = getId id,
        BeamIC.onCreateIssueMsgs = show <$> onCreateIssueMsgs,
        BeamIC.onAutoMarkIssueResMsgs = show <$> onAutoMarkIssueResMsgs,
        BeamIC.onIssueReopenMsgs = show <$> onIssueReopenMsgs,
        BeamIC.onKaptMarkIssueAwtMsgs = show <$> onKaptMarkIssueAwtMsgs,
        ..
      }
