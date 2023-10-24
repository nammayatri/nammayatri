{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module IssueManagement.Storage.Queries.Issue.IssueConfig where

import IssueManagement.Domain.Types.Issue.IssueConfig
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIC
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

findOne :: BeamFlow m => m (Maybe IssueConfig)
findOne = findOneWithKV [Is BeamIC.id $ Not $ Eq ""]

instance FromTType' BeamIC.IssueConfig IssueConfig where
  fromTType' BeamIC.IssueConfigT {..} = do
    pure $
      Just
        IssueConfig
          { id = Id id,
            onCreateIssueMsgs = Id <$> onCreateIssueMsgs,
            onAutoMarkIssueClsMsgs = Id <$> onAutoMarkIssueClsMsgs,
            onIssueReopenMsgs = Id <$> onIssueReopenMsgs,
            onKaptMarkIssueResMsgs = Id <$> onKaptMarkIssueResMsgs,
            ..
          }

instance ToTType' BeamIC.IssueConfig IssueConfig where
  toTType' IssueConfig {..} = do
    BeamIC.IssueConfigT
      { BeamIC.id = getId id,
        BeamIC.onCreateIssueMsgs = show <$> onCreateIssueMsgs,
        BeamIC.onAutoMarkIssueClsMsgs = show <$> onAutoMarkIssueClsMsgs,
        BeamIC.onIssueReopenMsgs = show <$> onIssueReopenMsgs,
        BeamIC.onKaptMarkIssueResMsgs = show <$> onKaptMarkIssueResMsgs,
        ..
      }
