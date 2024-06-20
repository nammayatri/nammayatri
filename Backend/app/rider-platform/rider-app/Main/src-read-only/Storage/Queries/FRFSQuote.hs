{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuote where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuote.FRFSQuote -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FRFSQuote.FRFSQuote] -> m ())
createMany = traverse_ create

findAllBySearchId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSSearch.FRFSSearch -> m [Domain.Types.FRFSQuote.FRFSQuote])
findAllBySearchId searchId = do findAllWithKV [Se.Is Beam.searchId $ Se.Eq (Kernel.Types.Id.getId searchId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSQuote.FRFSQuote))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FRFSQuote.FRFSQuote -> m (Maybe Domain.Types.FRFSQuote.FRFSQuote))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FRFSQuote.FRFSQuote -> m ())
updateByPrimaryKey (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam._type _type,
      Se.Set Beam.fromStationId (Kernel.Types.Id.getId fromStationId),
      Se.Set Beam.toStationId (Kernel.Types.Id.getId toStationId),
      Se.Set Beam.quantity quantity,
      Se.Set Beam.stationsJson stationsJson,
      Se.Set Beam.vehicleType vehicleType,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.currency ((Kernel.Prelude.Just . (.currency)) price),
      Se.Set Beam.price ((.amount) price),
      Se.Set Beam.validTill validTill,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  fromTType' (Beam.FRFSQuoteT {..}) = do
    pure $
      Just
        Domain.Types.FRFSQuote.FRFSQuote
          { id = Kernel.Types.Id.Id id,
            bppItemId = bppItemId,
            searchId = Kernel.Types.Id.Id searchId,
            _type = _type,
            fromStationId = Kernel.Types.Id.Id fromStationId,
            toStationId = Kernel.Types.Id.Id toStationId,
            quantity = quantity,
            stationsJson = stationsJson,
            vehicleType = vehicleType,
            bppSubscriberId = bppSubscriberId,
            bppSubscriberUrl = bppSubscriberUrl,
            providerId = providerId,
            providerName = providerName,
            providerDescription = providerDescription,
            price = Kernel.Types.Common.mkPrice currency price,
            validTill = validTill,
            riderId = Kernel.Types.Id.Id riderId,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            partnerOrgTransactionId = Kernel.Types.Id.Id <$> partnerOrgTransactionId,
            partnerOrgId = Kernel.Types.Id.Id <$> partnerOrgId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FRFSQuote Domain.Types.FRFSQuote.FRFSQuote where
  toTType' (Domain.Types.FRFSQuote.FRFSQuote {..}) = do
    Beam.FRFSQuoteT
      { Beam.id = Kernel.Types.Id.getId id,
        Beam.bppItemId = bppItemId,
        Beam.searchId = Kernel.Types.Id.getId searchId,
        Beam._type = _type,
        Beam.fromStationId = Kernel.Types.Id.getId fromStationId,
        Beam.toStationId = Kernel.Types.Id.getId toStationId,
        Beam.quantity = quantity,
        Beam.stationsJson = stationsJson,
        Beam.vehicleType = vehicleType,
        Beam.bppSubscriberId = bppSubscriberId,
        Beam.bppSubscriberUrl = bppSubscriberUrl,
        Beam.providerId = providerId,
        Beam.providerName = providerName,
        Beam.providerDescription = providerDescription,
        Beam.currency = (Kernel.Prelude.Just . (.currency)) price,
        Beam.price = (.amount) price,
        Beam.validTill = validTill,
        Beam.riderId = Kernel.Types.Id.getId riderId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.partnerOrgTransactionId = Kernel.Types.Id.getId <$> partnerOrgTransactionId,
        Beam.partnerOrgId = Kernel.Types.Id.getId <$> partnerOrgId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
