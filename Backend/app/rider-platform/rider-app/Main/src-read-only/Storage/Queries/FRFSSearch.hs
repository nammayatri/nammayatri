{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSSearch where

import qualified Domain.Types.FRFSSearch
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude hiding (sequence)
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSSearch as Beam
import Storage.Queries.Transformers.FRFSSearch

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSSearch.FRFSSearch] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

getTicketPlaces :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FRFSSearch.FRFSSearch])
getTicketPlaces merchantOperatingCityId = do findAllWithKV [Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m (Maybe Domain.Types.FRFSSearch.FRFSSearch))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSSearch.FRFSSearch -> m ())
updateByPrimaryKey (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.agency (journeyLegInfo >>= (.agency)),
      Se.Set Beam.convenienceCost (Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo),
      Se.Set Beam.journeyId (Kernel.Prelude.fmap (Kernel.Types.Id.getId . (.journeyId)) journeyLegInfo),
      Se.Set Beam.journeyLegOrder (Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo),
      Se.Set Beam.skipBooking (Kernel.Prelude.fmap (.skipBooking) journeyLegInfo),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  fromTType' (Beam.FRFSSearchT {..}) = do
    pure $
      Just
        Domain.Types.FRFSSearch.FRFSSearch
          { fromStationId = Kernel.Types.Id.Id fromStationId,
            id = Kernel.Types.Id.Id id,
            journeyLegInfo = mkJourneyLegInfo agency convenienceCost journeyId journeyLegOrder skipBooking,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            quantity = quantity,
            riderId = Kernel.Types.Id.Id riderId,
            toStationId = Kernel.Types.Id.Id toStationId,
            vehicleType = vehicleType,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSSearch Domain.Types.FRFSSearch.FRFSSearch where
  toTType' (Domain.Types.FRFSSearch.FRFSSearch {..}) = do
    Beam.FRFSSearchT
      { Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.agency = journeyLegInfo >>= (.agency),
        Beam.convenienceCost = Kernel.Prelude.fmap (.convenienceCost) journeyLegInfo,
        Beam.journeyId = Kernel.Prelude.fmap (Kernel.Types.Id.getId . (.journeyId)) journeyLegInfo,
        Beam.journeyLegOrder = Kernel.Prelude.fmap (.journeyLegOrder) journeyLegInfo,
        Beam.skipBooking = Kernel.Prelude.fmap (.skipBooking) journeyLegInfo,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.quantity = quantity,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.vehicleType = vehicleType,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
