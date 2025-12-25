{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FRFSQuote (module Storage.Queries.FRFSQuote, module ReExport) where

import qualified Domain.Types.FRFSQuote
import qualified Domain.Types.FRFSSearch
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FRFSQuote as Beam
import Storage.Queries.FRFSQuoteExtra as ReExport

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
    [ Se.Set Beam._type _type,
      Se.Set Beam.bppDelayedInterest bppDelayedInterest,
      Se.Set Beam.bppItemId bppItemId,
      Se.Set Beam.bppSubscriberId bppSubscriberId,
      Se.Set Beam.bppSubscriberUrl bppSubscriberUrl,
      Se.Set Beam.busLocationData (Just $ toJSON busLocationData),
      Se.Set Beam.discountedTickets discountedTickets,
      Se.Set Beam.eventDiscountAmount eventDiscountAmount,
      Se.Set Beam.appSession (fareDetails <&> (.appSession)),
      Se.Set Beam.distance (fareDetails <&> (.distance)),
      Se.Set Beam.providerRouteId (fareDetails <&> (.providerRouteId)),
      Se.Set Beam.ticketTypeCode (fareDetails <&> (.ticketTypeCode)),
      Se.Set Beam.trainTypeCode (fareDetails <&> (.trainTypeCode)),
      Se.Set Beam.via (fareDetails <&> (.via)),
      Se.Set Beam.fromStationAddress fromStationAddress,
      Se.Set Beam.fromStationId fromStationCode,
      Se.Set Beam.fromStationName fromStationName,
      Se.Set Beam.fromStationLat ((.lat) <$> fromStationPoint),
      Se.Set Beam.fromStationLon ((.lon) <$> fromStationPoint),
      Se.Set Beam.integratedBppConfigId (Kernel.Types.Id.getId integratedBppConfigId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.multimodalSearchRequestId multimodalSearchRequestId,
      Se.Set Beam.oldCacheDump oldCacheDump,
      Se.Set Beam.partnerOrgId (Kernel.Types.Id.getId <$> partnerOrgId),
      Se.Set Beam.partnerOrgTransactionId (Kernel.Types.Id.getId <$> partnerOrgTransactionId),
      Se.Set Beam.providerDescription providerDescription,
      Se.Set Beam.providerId providerId,
      Se.Set Beam.providerName providerName,
      Se.Set Beam.riderId (Kernel.Types.Id.getId riderId),
      Se.Set Beam.routeStationsJson routeStationsJson,
      Se.Set Beam.searchId (Kernel.Types.Id.getId searchId),
      Se.Set Beam.stationsJson stationsJson,
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
