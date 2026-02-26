{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSSearch (module Storage.Queries.FRFSSearch, module ReExport) where

import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import Storage.Queries.FRFSSearchExtra as ReExport

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSSearch.FRFSSearch] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

getTicketPlaces :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FRFSSearch.FRFSSearch])
getTicketPlaces merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

updateIsOnSearchReceivedById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateIsOnSearchReceivedById isOnSearchReceived id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.isOnSearchReceived isOnSearchReceived, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateRiderIdById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateRiderIdById riderId id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.riderId (Kernel.Types.Id.getId riderId), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateByPrimaryKey (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.busLocationData (Just $ toJSON busLocationData),
      Se.Set Beam.fromStationAddress fromStationAddress,
      Se.Set Beam.fromStationId fromStationCode,
      Se.Set Beam.fromStationName fromStationName,
      Se.Set Beam.fromStationLat ((.lat) <$> fromStationPoint),
      Se.Set Beam.fromStationLon ((.lon) <$> fromStationPoint),
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.isOnSearchReceived isOnSearchReceived,
      Se.Set Beam.isSingleMode isSingleMode,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.multimodalSearchRequestId multimodalSearchRequestId,
      Se.Set Beam.onSearchFailed onSearchFailed,
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeId routeCode,
      Se.Set Beam.searchAsParentStops searchAsParentStops,
      Se.Set Beam.toStationAddress toStationAddress,
      Se.Set Beam.toStationId toStationCode,
      Se.Set Beam.toStationName toStationName,
      Se.Set Beam.toStationLat ((.lat) <$> toStationPoint),
      Se.Set Beam.toStationLon ((.lon) <$> toStationPoint),
      Se.Set Beam.validTill validTill,
      Se.Set Beam.vehicleNumber vehicleNumber,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
