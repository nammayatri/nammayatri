{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AlertRequest where

import qualified Data.Text
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import qualified Domain.Types.AlertRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AlertRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AlertRequest.AlertRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.AlertRequest.AlertRequest] -> m ())
createMany = traverse_ create

updateStatusWithReason ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest -> m ())
updateStatusWithReason status reason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.reason reason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.AlertRequest.AlertRequest -> m (Maybe Domain.Types.AlertRequest.AlertRequest))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.AlertRequest.AlertRequest -> m ())
updateByPrimaryKey (Domain.Types.AlertRequest.AlertRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.body body,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reason reason,
      Se.Set Beam.requestData requestData,
      Se.Set Beam.requestType (Kernel.Prelude.Just requestType),
      Se.Set Beam.requesteeId (Kernel.Types.Id.getId requesteeId),
      Se.Set Beam.requesteeType (Kernel.Prelude.Just requesteeType),
      Se.Set Beam.requestorId (Kernel.Types.Id.getId requestorId),
      Se.Set Beam.requestorType (Kernel.Prelude.Just requestorType),
      Se.Set Beam.status status,
      Se.Set Beam.title title,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.AlertRequest Domain.Types.AlertRequest.AlertRequest where
  fromTType' (Beam.AlertRequestT {..}) = do
    pure $
      Just
        Domain.Types.AlertRequest.AlertRequest
          { body = body,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            requestData = requestData,
            requestType = Kernel.Prelude.fromMaybe Domain.Types.Alert.AlertRequestType.EndRideApproval requestType,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requesteeType = Kernel.Prelude.fromMaybe Domain.Types.AlertRequest.FleetOwner requesteeType,
            requestorId = Kernel.Types.Id.Id requestorId,
            requestorType = Kernel.Prelude.fromMaybe Domain.Types.AlertRequest.DriverGenerated requestorType,
            status = status,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AlertRequest Domain.Types.AlertRequest.AlertRequest where
  toTType' (Domain.Types.AlertRequest.AlertRequest {..}) = do
    Beam.AlertRequestT
      { Beam.body = body,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reason = reason,
        Beam.requestData = requestData,
        Beam.requestType = Kernel.Prelude.Just requestType,
        Beam.requesteeId = Kernel.Types.Id.getId requesteeId,
        Beam.requesteeType = Kernel.Prelude.Just requesteeType,
        Beam.requestorId = Kernel.Types.Id.getId requestorId,
        Beam.requestorType = Kernel.Prelude.Just requestorType,
        Beam.status = status,
        Beam.title = title,
        Beam.updatedAt = updatedAt
      }
