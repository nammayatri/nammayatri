{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AdditionalInfoRequestExtra where

import Domain.Types.AdditionalInfoRequest
import qualified Domain.Types.OperationHubRequests as OHR
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow)
import qualified Sequelize as Se
import qualified Storage.Beam.AdditionalInfoRequest as BeamAIR

-- Extra code goes here --

findAllByOperationHubRequestIdWithStatus ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id OHR.OperationHubRequests ->
  Maybe AdditionalInfoStatus ->
  Maybe Int ->
  Maybe Int ->
  m [AdditionalInfoRequest]
findAllByOperationHubRequestIdWithStatus opHubReqId mbStatus mbLimit mbOffset = do
  let filters =
        [ Se.Is BeamAIR.operationHubRequestId $ Se.Eq (getId opHubReqId)
        ]
          <> maybe [] (\s -> [Se.Is BeamAIR.status $ Se.Eq s]) mbStatus
  findAllWithOptionsKV
    [Se.And filters]
    (Se.Desc BeamAIR.createdAt)
    mbLimit
    (Just $ fromMaybe 0 mbOffset)

countByOperationHubRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id OHR.OperationHubRequests ->
  Maybe AdditionalInfoStatus ->
  m Int
countByOperationHubRequestId opHubReqId mbStatus = do
  let filters =
        [ Se.Is BeamAIR.operationHubRequestId $ Se.Eq (getId opHubReqId)
        ]
          <> maybe [] (\s -> [Se.Is BeamAIR.status $ Se.Eq s]) mbStatus
  results <-
    findAllWithKV
      [Se.And filters]
  pure $ length results

findPendingByOperationHubRequestId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Id OHR.OperationHubRequests ->
  m [AdditionalInfoRequest]
findPendingByOperationHubRequestId opHubReqId = do
  findAllWithKV
    [ Se.And
        [ Se.Is BeamAIR.operationHubRequestId $ Se.Eq (getId opHubReqId),
          Se.Is BeamAIR.status $ Se.Eq PENDING
        ]
    ]
