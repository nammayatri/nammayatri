{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.BookingLocation where

import qualified Domain.Types.BookingLocation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.BookingLocation as Beam
import Storage.Queries.Transformers.BookingLocation

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingLocation.BookingLocation -> m ())
create = createWithKV

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.BookingLocation.BookingLocation -> m (Maybe Domain.Types.BookingLocation.BookingLocation))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

updateAddress :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.BookingLocation.LocationAddress -> Kernel.Types.Id.Id Domain.Types.BookingLocation.BookingLocation -> m ())
updateAddress address id = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.area ((.area) address),
      Se.Set Beam.areaCode ((.areaCode) address),
      Se.Set Beam.building ((.building) address),
      Se.Set Beam.city ((.city) address),
      Se.Set Beam.country ((.country) address),
      Se.Set Beam.door ((.door) address),
      Se.Set Beam.extras ((.extras) address),
      Se.Set Beam.instructions ((.instructions) address),
      Se.Set Beam.state ((.state) address),
      Se.Set Beam.street ((.street) address),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

instance FromTType' Beam.BookingLocation Domain.Types.BookingLocation.BookingLocation where
  fromTType' (Beam.BookingLocationT {..}) = do
    pure $
      Just
        Domain.Types.BookingLocation.BookingLocation
          { address = mkLocationAddress area areaCode building city country door extras instructions state street,
            createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lat = lat,
            lon = lon,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.BookingLocation Domain.Types.BookingLocation.BookingLocation where
  toTType' (Domain.Types.BookingLocation.BookingLocation {..}) = do
    Beam.BookingLocationT
      { Beam.area = (.area) address,
        Beam.areaCode = (.areaCode) address,
        Beam.building = (.building) address,
        Beam.city = (.city) address,
        Beam.country = (.country) address,
        Beam.door = (.door) address,
        Beam.extras = (.extras) address,
        Beam.instructions = (.instructions) address,
        Beam.state = (.state) address,
        Beam.street = (.street) address,
        Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lat = lat,
        Beam.lon = lon,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
