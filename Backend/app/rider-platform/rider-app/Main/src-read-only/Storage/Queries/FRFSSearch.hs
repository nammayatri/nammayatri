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
import qualified Lib.JourneyLeg.Types
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import Storage.Queries.FRFSSearchExtra as ReExport
import Storage.Queries.Transformers.FRFSSearch

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

updateJourneyLegStatus :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Lib.JourneyLeg.Types.JourneyLegStatus -> Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateJourneyLegStatus journeyLegStatus id = do
  _now <- getCurrentTime
  updateOneWithKV [Se.Set Beam.journeyLegStatus journeyLegStatus, Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

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
    [ Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId <$> integratedBppConfigId),
      Se.Set Beam.isOnSearchReceived isOnSearchReceived,
      Se.Set Beam.agency (journeyLegInfo >>= (.agency)),
      Se.Set Beam.convenienceCost (Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo),
      Se.Set Beam.isDeleted (journeyLegInfo >>= (.isDeleted)),
      Se.Set Beam.journeyId (Kernel.Prelude.fmap (.journeyId) journeyLegInfo),
      Se.Set Beam.journeyLegOrder (Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo),
      Se.Set Beam.onSearchFailed (journeyLegInfo >>= (.onSearchFailed)),
      Se.Set Beam.pricingId (journeyLegInfo >>= (.pricingId)),
      Se.Set Beam.skipBooking (Kernel.Prelude.fmap (.skipBooking) journeyLegInfo),
      Se.Set Beam.journeyLegStatus journeyLegStatus,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.recentLocationId (Kernel.Types.Id.getId <$> recentLocationId),
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeId (Kernel.Types.Id.getId <$> routeId),
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
