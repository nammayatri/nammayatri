{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module IssueManagement.Storage.Queries.Issue.IssueConfig where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueConfig
import qualified IssueManagement.Storage.Beam.Issue.IssueConfig as BeamIC
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => IssueConfig -> m ()
create = createWithKV

findByMerchantOpCityId :: BeamFlow m r => Id MerchantOperatingCity -> m (Maybe IssueConfig)
findByMerchantOpCityId (Id merchantOpCityId) = findOneWithKV [Is BeamIC.merchantOperatingCityId $ Eq merchantOpCityId]

instance FromTType' BeamIC.IssueConfig IssueConfig where
  fromTType' BeamIC.IssueConfigT {..} = do
    pure $
      Just
        IssueConfig
          { id = Id id,
            merchantOperatingCityId = Id merchantOperatingCityId,
            onCreateIssueMsgs = Id <$> onCreateIssueMsgs,
            onAutoMarkIssueClsMsgs = Id <$> onAutoMarkIssueClsMsgs,
            onIssueReopenMsgs = Id <$> onIssueReopenMsgs,
            onKaptMarkIssueResMsgs = Id <$> onKaptMarkIssueResMsgs,
            merchantId = Id merchantId,
            onIssueCloseMsgs = Id <$> onIssueCloseMsgs,
            ..
          }

instance ToTType' BeamIC.IssueConfig IssueConfig where
  toTType' IssueConfig {..} = do
    BeamIC.IssueConfigT
      { BeamIC.id = getId id,
        BeamIC.merchantOperatingCityId = getId merchantOperatingCityId,
        BeamIC.onCreateIssueMsgs = getId <$> onCreateIssueMsgs,
        BeamIC.onAutoMarkIssueClsMsgs = getId <$> onAutoMarkIssueClsMsgs,
        BeamIC.onIssueReopenMsgs = getId <$> onIssueReopenMsgs,
        BeamIC.onKaptMarkIssueResMsgs = getId <$> onKaptMarkIssueResMsgs,
        BeamIC.merchantId = getId merchantId,
        BeamIC.onIssueCloseMsgs = getId <$> onIssueCloseMsgs,
        ..
      }
