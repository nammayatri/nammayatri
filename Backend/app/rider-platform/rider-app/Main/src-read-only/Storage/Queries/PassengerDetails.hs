{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PassengerDetails where

import qualified Domain.Types.PassengerDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PassengerDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassengerDetails.PassengerDetails -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PassengerDetails.PassengerDetails] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PassengerDetails.PassengerDetails -> m (Maybe Domain.Types.PassengerDetails.PassengerDetails))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PassengerDetails.PassengerDetails -> m ())
updateByPrimaryKey (Domain.Types.PassengerDetails.PassengerDetails {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.age age,
      Se.Set Beam.bookingId bookingId,
      Se.Set Beam.firstName firstName,
      Se.Set Beam.lastName lastName,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PassengerDetails Domain.Types.PassengerDetails.PassengerDetails where
  fromTType' (Beam.PassengerDetailsT {..}) = do
    pure $
      Just
        Domain.Types.PassengerDetails.PassengerDetails
          { age = age,
            bookingId = bookingId,
            firstName = firstName,
            id = Kernel.Types.Id.Id id,
            lastName = lastName,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PassengerDetails Domain.Types.PassengerDetails.PassengerDetails where
  toTType' (Domain.Types.PassengerDetails.PassengerDetails {..}) = do
    Beam.PassengerDetailsT
      { Beam.age = age,
        Beam.bookingId = bookingId,
        Beam.firstName = firstName,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastName = lastName,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
