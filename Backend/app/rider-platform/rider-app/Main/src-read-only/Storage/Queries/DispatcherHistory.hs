{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DispatcherHistory where

import qualified Domain.Types.DispatcherHistory
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DispatcherHistory as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DispatcherHistory.DispatcherHistory -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DispatcherHistory.DispatcherHistory] -> m ())
createMany = traverse_ create

findByDispatcherId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.DispatcherHistory.DispatcherHistory]))
findByDispatcherId limit offset dispatcherId = do findAllWithOptionsDb [Se.And [Se.Is Beam.dispatcherId $ Se.Eq (Kernel.Types.Id.getId dispatcherId)]] (Se.Desc Beam.createdAt) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DispatcherHistory.DispatcherHistory -> m (Maybe Domain.Types.DispatcherHistory.DispatcherHistory))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DispatcherHistory.DispatcherHistory -> m (Maybe Domain.Types.DispatcherHistory.DispatcherHistory))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DispatcherHistory.DispatcherHistory -> m ())
updateByPrimaryKey (Domain.Types.DispatcherHistory.DispatcherHistory {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.conductorCode conductorCode,
      Se.Set Beam.currentVehicle currentVehicle,
      Se.Set Beam.depotId depotId,
      Se.Set Beam.dispatcherId (Kernel.Types.Id.getId dispatcherId),
      Se.Set Beam.driverCode driverCode,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.reasonContent reasonContent,
      Se.Set Beam.reasonTag reasonTag,
      Se.Set Beam.replacedVehicle replacedVehicle,
      Se.Set Beam.updatedAt _now,
      Se.Set Beam.waybillNo waybillNo
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.DispatcherHistory Domain.Types.DispatcherHistory.DispatcherHistory where
  fromTType' (Beam.DispatcherHistoryT {..}) = do
    pure $
      Just
        Domain.Types.DispatcherHistory.DispatcherHistory
          { conductorCode = conductorCode,
            createdAt = createdAt,
            currentVehicle = currentVehicle,
            depotId = depotId,
            dispatcherId = Kernel.Types.Id.Id dispatcherId,
            driverCode = driverCode,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            reasonContent = reasonContent,
            reasonTag = reasonTag,
            replacedVehicle = replacedVehicle,
            updatedAt = updatedAt,
            waybillNo = waybillNo
          }

instance ToTType' Beam.DispatcherHistory Domain.Types.DispatcherHistory.DispatcherHistory where
  toTType' (Domain.Types.DispatcherHistory.DispatcherHistory {..}) = do
    Beam.DispatcherHistoryT
      { Beam.conductorCode = conductorCode,
        Beam.createdAt = createdAt,
        Beam.currentVehicle = currentVehicle,
        Beam.depotId = depotId,
        Beam.dispatcherId = Kernel.Types.Id.getId dispatcherId,
        Beam.driverCode = driverCode,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.reasonContent = reasonContent,
        Beam.reasonTag = reasonTag,
        Beam.replacedVehicle = replacedVehicle,
        Beam.updatedAt = updatedAt,
        Beam.waybillNo = waybillNo
      }
