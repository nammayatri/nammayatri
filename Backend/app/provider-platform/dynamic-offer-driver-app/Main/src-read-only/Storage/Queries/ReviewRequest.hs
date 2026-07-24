{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ReviewRequest (module Storage.Queries.ReviewRequest, module ReExport) where

import qualified Data.Aeson
import qualified Domain.Types.ReviewRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ReviewRequest as Beam
import Storage.Queries.ReviewRequestExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReviewRequest.ReviewRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.ReviewRequest.ReviewRequest] -> m ())
createMany = traverse_ create

findByEntityIdAndEntityTypeAndRequestTypeAndStatusAndRcNo ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Domain.Types.ReviewRequest.EntityType -> Domain.Types.ReviewRequest.RequestType -> Domain.Types.ReviewRequest.RequestStatus -> Kernel.Prelude.Maybe Kernel.Prelude.Text -> m [Domain.Types.ReviewRequest.ReviewRequest])
findByEntityIdAndEntityTypeAndRequestTypeAndStatusAndRcNo entityId entityType requestType requestStatus rcNo = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.entityId $ Se.Eq entityId,
          Se.Is Beam.entityType $ Se.Eq entityType,
          Se.Is Beam.requestType $ Se.Eq requestType,
          Se.Is Beam.requestStatus $ Se.Eq requestStatus,
          Se.Is Beam.rcNo $ Se.Eq rcNo
        ]
    ]

updateRequestStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ReviewRequest.ReviewRequest -> Domain.Types.ReviewRequest.RequestStatus -> m ())
updateRequestStatus id requestStatus = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id), Se.Is Beam.requestStatus $ Se.Eq requestStatus]]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.ReviewRequest.ReviewRequest -> m (Maybe Domain.Types.ReviewRequest.ReviewRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ReviewRequest.ReviewRequest -> m ())
updateByPrimaryKey (Domain.Types.ReviewRequest.ReviewRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.documentDetails (Data.Aeson.toJSON <$> documentDetails),
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.rcNo rcNo,
      Se.Set Beam.requestStatus requestStatus,
      Se.Set Beam.requestType requestType,
      Se.Set Beam.reviewerId (Kernel.Types.Id.getId <$> reviewerId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
