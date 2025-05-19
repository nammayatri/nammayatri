{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSSearchRequest where

import qualified Domain.Types.FRFSSearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearchRequest as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearchRequest.FRFSSearchRequest -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSSearchRequest.FRFSSearchRequest] -> m ())
createMany = traverse_ create

findAllByBapId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ([Domain.Types.FRFSSearchRequest.FRFSSearchRequest]))
findAllByBapId bapId = do findAllWithKV [Se.Is Beam.bapId $ Se.Eq bapId]

findByTrasactionId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearchRequest.FRFSSearchRequest -> m (Maybe Domain.Types.FRFSSearchRequest.FRFSSearchRequest))
findByTrasactionId transactionId = do findOneWithKV [Se.Is Beam.transactionId $ Se.Eq (Kernel.Types.Id.getId transactionId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearchRequest.FRFSSearchRequest -> m (Maybe Domain.Types.FRFSSearchRequest.FRFSSearchRequest))
findByPrimaryKey transactionId = do findOneWithKV [Se.And [Se.Is Beam.transactionId $ Se.Eq (Kernel.Types.Id.getId transactionId)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearchRequest.FRFSSearchRequest -> m ())
updateByPrimaryKey (Domain.Types.FRFSSearchRequest.FRFSSearchRequest {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bapId bapId,
      Se.Set Beam.bapUri bapUri,
      Se.Set Beam.bppId bppId,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.destinationStationId (Kernel.Types.Id.getId <$> destinationStationId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.sourceStationId (Kernel.Types.Id.getId <$> sourceStationId),
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId)
    ]
    [Se.And [Se.Is Beam.transactionId $ Se.Eq (Kernel.Types.Id.getId transactionId)]]

instance FromTType' Beam.FRFSSearchRequest Domain.Types.FRFSSearchRequest.FRFSSearchRequest where
  fromTType' (Beam.FRFSSearchRequestT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSearchRequest.FRFSSearchRequest
          { bapId = bapId,
            bapUri = bapUri,
            bppId = bppId,
            createdAt = createdAt,
            destinationStationId = Kernel.Types.Id.Id <$> destinationStationId,
            quantity = quantity,
            sourceStationId = Kernel.Types.Id.Id <$> sourceStationId,
            transactionId = Kernel.Types.Id.Id transactionId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.FRFSSearchRequest Domain.Types.FRFSSearchRequest.FRFSSearchRequest where
  toTType' (Domain.Types.FRFSSearchRequest.FRFSSearchRequest {..}) = do
    Beam.FRFSSearchRequestT
      { Beam.bapId = bapId,
        Beam.bapUri = bapUri,
        Beam.bppId = bppId,
        Beam.createdAt = createdAt,
        Beam.destinationStationId = Kernel.Types.Id.getId <$> destinationStationId,
        Beam.quantity = quantity,
        Beam.sourceStationId = Kernel.Types.Id.getId <$> sourceStationId,
        Beam.transactionId = Kernel.Types.Id.getId transactionId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
