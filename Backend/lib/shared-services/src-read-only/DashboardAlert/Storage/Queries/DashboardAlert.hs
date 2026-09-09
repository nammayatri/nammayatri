{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module DashboardAlert.Storage.Queries.DashboardAlert where

import qualified DashboardAlert.Domain.Types.Common
import qualified DashboardAlert.Domain.Types.DashboardAlert
import qualified DashboardAlert.Storage.Beam.DashboardAlert as Beam
import qualified DashboardAlert.Storage.BeamFlow
import qualified Data.Text
import qualified Domain.Types.Alert.AlertRequestStatus
import qualified Domain.Types.Alert.AlertRequestType
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se

create :: (DashboardAlert.Storage.BeamFlow.BeamFlow m r) => (DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert -> m ())
create = createWithKV

createMany :: (DashboardAlert.Storage.BeamFlow.BeamFlow m r) => ([DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert] -> m ())
createMany = traverse_ create

findAllByRequesteeId ::
  (DashboardAlert.Storage.BeamFlow.BeamFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id DashboardAlert.Domain.Types.Common.Person -> m ([DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert]))
findAllByRequesteeId limit offset requesteeId = do findAllWithOptionsKV [Se.Is Beam.requesteeId $ Se.Eq (Kernel.Types.Id.getId requesteeId)] (Se.Desc Beam.createdAt) limit offset

updateStatusWithReason ::
  (DashboardAlert.Storage.BeamFlow.BeamFlow m r) =>
  (Domain.Types.Alert.AlertRequestStatus.AlertRequestStatus -> Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Types.Id.Id DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert -> m ())
updateStatusWithReason status reason id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.status status, Se.Set Beam.reason reason, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey ::
  (DashboardAlert.Storage.BeamFlow.BeamFlow m r) =>
  (Kernel.Types.Id.Id DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert -> m (Maybe DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (DashboardAlert.Storage.BeamFlow.BeamFlow m r) => (DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert -> m ())
updateByPrimaryKey (DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.body body,
      Se.Set Beam.entityId entityId,
      Se.Set Beam.entityType entityType,
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

instance FromTType' Beam.DashboardAlert DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert where
  fromTType' (Beam.DashboardAlertT {..}) = do
    pure $
      Just
        DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert
          { body = body,
            createdAt = createdAt,
            entityId = entityId,
            entityType = entityType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reason = reason,
            requestData = requestData,
            requestType = Kernel.Prelude.fromMaybe Domain.Types.Alert.AlertRequestType.EndRideApproval requestType,
            requesteeId = Kernel.Types.Id.Id requesteeId,
            requesteeType = Kernel.Prelude.fromMaybe DashboardAlert.Domain.Types.DashboardAlert.FleetOwner requesteeType,
            requestorId = Kernel.Types.Id.Id requestorId,
            requestorType = Kernel.Prelude.fromMaybe DashboardAlert.Domain.Types.DashboardAlert.DriverGenerated requestorType,
            status = status,
            title = title,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DashboardAlert DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert where
  toTType' (DashboardAlert.Domain.Types.DashboardAlert.DashboardAlert {..}) = do
    Beam.DashboardAlertT
      { Beam.body = body,
        Beam.createdAt = createdAt,
        Beam.entityId = entityId,
        Beam.entityType = entityType,
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
