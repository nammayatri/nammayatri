{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DigiLockerLogs where

import qualified Domain.Types.DigiLockerLogs
import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DigiLockerLogs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DigiLockerLogs.DigiLockerLogs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DigiLockerLogs.DigiLockerLogs] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DigiLockerLogs.DigiLockerLogs]))
findAllByDriverId limit offset driverId = do findAllWithOptionsKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)] (Se.Desc Beam.createdAt) limit offset

findAllByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType -> m ([Domain.Types.DigiLockerLogs.DigiLockerLogs]))
findAllByDriverIdAndDocType limit offset driverId docType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.docType $ Se.Eq docType
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findAllByDriverIdAndFlowType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Text -> m ([Domain.Types.DigiLockerLogs.DigiLockerLogs]))
findAllByDriverIdAndFlowType limit offset driverId flowType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.flowType $ Se.Eq flowType
        ]
    ]
    (Se.Desc Beam.createdAt)
    limit
    offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DigiLockerLogs.DigiLockerLogs -> m (Maybe Domain.Types.DigiLockerLogs.DigiLockerLogs))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DigiLockerLogs.DigiLockerLogs -> m ())
updateByPrimaryKey (Domain.Types.DigiLockerLogs.DigiLockerLogs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.createdAt createdAt,
      Se.Set Beam.digiLockerState digiLockerState,
      Se.Set Beam.digiLockerUri digiLockerUri,
      Se.Set Beam.docType docType,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.errorCode errorCode,
      Se.Set Beam.errorDescription errorDescription,
      Se.Set Beam.flowType flowType,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.requestPayload requestPayload,
      Se.Set Beam.responsePayload responsePayload,
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DigiLockerLogs Domain.Types.DigiLockerLogs.DigiLockerLogs where
  fromTType' (Beam.DigiLockerLogsT {..}) = do
    pure $
      Just
        Domain.Types.DigiLockerLogs.DigiLockerLogs
          { createdAt = createdAt,
            digiLockerState = digiLockerState,
            digiLockerUri = digiLockerUri,
            docType = docType,
            driverId = Kernel.Types.Id.Id driverId,
            errorCode = errorCode,
            errorDescription = errorDescription,
            flowType = flowType,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            requestPayload = requestPayload,
            responsePayload = responsePayload,
            status = status,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DigiLockerLogs Domain.Types.DigiLockerLogs.DigiLockerLogs where
  toTType' (Domain.Types.DigiLockerLogs.DigiLockerLogs {..}) = do
    Beam.DigiLockerLogsT
      { Beam.createdAt = createdAt,
        Beam.digiLockerState = digiLockerState,
        Beam.digiLockerUri = digiLockerUri,
        Beam.docType = docType,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.errorCode = errorCode,
        Beam.errorDescription = errorDescription,
        Beam.flowType = flowType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.requestPayload = requestPayload,
        Beam.responsePayload = responsePayload,
        Beam.status = status,
        Beam.updatedAt = updatedAt
      }
