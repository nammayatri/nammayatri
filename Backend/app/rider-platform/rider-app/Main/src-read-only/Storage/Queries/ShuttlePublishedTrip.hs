{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ShuttlePublishedTrip where

import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.ShuttlePublishedTrip
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ShuttlePublishedTrip as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip] -> m ()
createMany = traverse_ create

deleteByFeedKeyAndTripIdAndDay :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Kernel.Prelude.Text -> (Kernel.Prelude.Text -> (Kernel.Prelude.Text -> m ()))
deleteByFeedKeyAndTripIdAndDay feedKey tripId day = do deleteWithKV [Se.And [Se.Is Beam.feedKey $ Se.Eq feedKey, Se.Is Beam.tripId $ Se.Eq tripId, Se.Is Beam.day $ Se.Eq day]]

findAllByFeedKeyAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip])
findAllByFeedKeyAndMerchantOperatingCityId feedKey merchantOperatingCityId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.feedKey $ Se.Eq feedKey,
          Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId merchantOperatingCityId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  Kernel.Prelude.Text ->
  (Kernel.Prelude.Text -> (Kernel.Prelude.Text -> m (Maybe Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip)))
findByPrimaryKey day feedKey tripId = do findOneWithKV [Se.And [Se.Is Beam.day $ Se.Eq day, Se.Is Beam.feedKey $ Se.Eq feedKey, Se.Is Beam.tripId $ Se.Eq tripId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip -> m ()
updateByPrimaryKey (Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.departure departure,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.riders riders,
      Se.Set Beam.routeCode routeCode,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.day $ Se.Eq day, Se.Is Beam.feedKey $ Se.Eq feedKey, Se.Is Beam.tripId $ Se.Eq tripId]]

instance FromTType' Beam.ShuttlePublishedTrip Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip where
  fromTType' (Beam.ShuttlePublishedTripT {..}) = do
    pure $
      Just
        Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip
          { createdAt = createdAt,
            day = day,
            departure = departure,
            feedKey = feedKey,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            riders = riders,
            routeCode = routeCode,
            tripId = tripId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ShuttlePublishedTrip Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip where
  toTType' (Domain.Types.ShuttlePublishedTrip.ShuttlePublishedTrip {..}) = do
    Beam.ShuttlePublishedTripT
      { Beam.createdAt = createdAt,
        Beam.day = day,
        Beam.departure = departure,
        Beam.feedKey = feedKey,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.riders = riders,
        Beam.routeCode = routeCode,
        Beam.tripId = tripId,
        Beam.updatedAt = updatedAt
      }
