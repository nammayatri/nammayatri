{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.HyperVergeSdkLogs where

import qualified Domain.Types.DocumentVerificationConfig
import qualified Domain.Types.HyperVergeSdkLogs
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.HyperVergeSdkLogs as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs] -> m ())
createMany = traverse_ create

findAllByDriverId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs]))
findAllByDriverId driverId = do findAllWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findAllByDriverIdAndDocType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Maybe Domain.Types.DocumentVerificationConfig.DocumentType -> m ([Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs]))
findAllByDriverIdAndDocType limit offset driverId docType = do
  findAllWithOptionsKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.docType $ Se.Eq docType
        ]
    ]
    (Se.Asc Beam.createdAt)
    limit
    offset

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m (Maybe Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs))
findByPrimaryKey txnId = do findOneWithKV [Se.And [Se.Is Beam.txnId $ Se.Eq txnId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs -> m ())
updateByPrimaryKey (Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.callbackResponse callbackResponse,
      Se.Set Beam.docType docType,
      Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.failureReason failureReason,
      Se.Set Beam.hvFlowId hvFlowId,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.status status,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.txnId $ Se.Eq txnId]]

instance FromTType' Beam.HyperVergeSdkLogs Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs where
  fromTType' (Beam.HyperVergeSdkLogsT {..}) = do
    pure $
      Just
        Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs
          { callbackResponse = callbackResponse,
            createdAt = createdAt,
            docType = docType,
            driverId = Kernel.Types.Id.Id driverId,
            failureReason = failureReason,
            hvFlowId = hvFlowId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            status = status,
            txnId = txnId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.HyperVergeSdkLogs Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs where
  toTType' (Domain.Types.HyperVergeSdkLogs.HyperVergeSdkLogs {..}) = do
    Beam.HyperVergeSdkLogsT
      { Beam.callbackResponse = callbackResponse,
        Beam.createdAt = createdAt,
        Beam.docType = docType,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.failureReason = failureReason,
        Beam.hvFlowId = hvFlowId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.status = status,
        Beam.txnId = txnId,
        Beam.updatedAt = updatedAt
      }
