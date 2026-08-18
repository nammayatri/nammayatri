{-# OPTIONS_GHC -Wno-orphans #-}

module IssueManagement.Storage.Queries.Issue.IssueApiIntegration where

import IssueManagement.Common
import IssueManagement.Domain.Types.Issue.IssueApiIntegration
import qualified IssueManagement.Storage.Beam.Issue.IssueApiIntegration as BeamAI
import IssueManagement.Storage.BeamFlow
import IssueManagement.Tools.UtilsTH
import Kernel.Types.Id

create :: BeamFlow m r => IssueApiIntegration -> m ()
create = createWithKV

findById :: BeamFlow m r => Id IssueApiIntegration -> m (Maybe IssueApiIntegration)
findById (Id apiIntegrationId) = findOneWithKV [Is BeamAI.id $ Eq apiIntegrationId]

findAllByMerchantId :: BeamFlow m r => Id Merchant -> m [IssueApiIntegration]
findAllByMerchantId (Id merchantId) =
  findAllWithOptionsKV [Is BeamAI.merchantId $ Eq merchantId] (Desc BeamAI.updatedAt) Nothing Nothing

updateByPrimaryKey :: BeamFlow m r => IssueApiIntegration -> m ()
updateByPrimaryKey IssueApiIntegration {..} =
  updateWithKV
    [ Set BeamAI.name name,
      Set BeamAI.description description,
      Set BeamAI.kind kind,
      Set BeamAI.method method,
      Set BeamAI.urlTemplate urlTemplate,
      Set BeamAI.headersJson headersJson,
      Set BeamAI.bodyTemplate bodyTemplate,
      Set BeamAI.timeoutMs timeoutMs,
      Set BeamAI.responseFieldsJson responseFieldsJson,
      Set BeamAI.isActive isActive,
      Set BeamAI.updatedAt updatedAt
    ]
    [Is BeamAI.id $ Eq (getId id)]

deleteById :: BeamFlow m r => Id IssueApiIntegration -> m ()
deleteById (Id apiIntegrationId) = deleteWithKV [Is BeamAI.id $ Eq apiIntegrationId]

instance FromTType' BeamAI.IssueApiIntegration IssueApiIntegration where
  fromTType' BeamAI.IssueApiIntegrationT {..} = do
    pure $
      Just
        IssueApiIntegration
          { id = Id id,
            merchantId = Id merchantId,
            ..
          }

instance ToTType' BeamAI.IssueApiIntegration IssueApiIntegration where
  toTType' IssueApiIntegration {..} = do
    BeamAI.IssueApiIntegrationT
      { BeamAI.id = getId id,
        BeamAI.merchantId = getId merchantId,
        BeamAI.name = name,
        BeamAI.description = description,
        BeamAI.kind = kind,
        BeamAI.method = method,
        BeamAI.urlTemplate = urlTemplate,
        BeamAI.headersJson = headersJson,
        BeamAI.bodyTemplate = bodyTemplate,
        BeamAI.timeoutMs = timeoutMs,
        BeamAI.responseFieldsJson = responseFieldsJson,
        BeamAI.isActive = isActive,
        BeamAI.createdAt = createdAt,
        BeamAI.updatedAt = updatedAt
      }
