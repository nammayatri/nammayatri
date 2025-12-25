{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RiderDriverCorrelation where

import qualified Domain.Types.Person
import qualified Domain.Types.RiderDetails
import qualified Domain.Types.RiderDriverCorrelation
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RiderDriverCorrelation as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation] -> m ())
createMany = traverse_ create

checkRiderFavDriver ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Prelude.Bool -> m (Maybe Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation))
checkRiderFavDriver riderDetailId driverId favourite = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId),
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.favourite $ Se.Eq favourite
        ]
    ]

findByRiderIdAndDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation))
findByRiderIdAndDriverId riderDetailId driverId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId),
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)
        ]
    ]

findFavDriversForRider ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> Kernel.Prelude.Bool -> m [Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation])
findFavDriversForRider riderDetailId favourite = do findAllWithKV [Se.And [Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId), Se.Is Beam.favourite $ Se.Eq favourite]]

updateFavouriteDriverForRider ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Bool -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateFavouriteDriverForRider favourite riderDetailId driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [Se.Set Beam.favourite favourite, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId),
          Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)
        ]
    ]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.RiderDetails.RiderDetails -> m (Maybe Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation))
findByPrimaryKey driverId riderDetailId = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId),
          Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId)
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation -> m ())
updateByPrimaryKey (Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.favourite favourite,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.mobileNumberEncrypted (mobileNumber & unEncrypted . encrypted),
      Se.Set Beam.mobileNumberHash (mobileNumber & hash),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId), Se.Is Beam.riderDetailId $ Se.Eq (Kernel.Types.Id.getId riderDetailId)]]

instance FromTType' Beam.RiderDriverCorrelation Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation where
  fromTType' (Beam.RiderDriverCorrelationT {..}) = do
    pure $
      Just
        Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation
          { createdAt = createdAt,
            driverId = Kernel.Types.Id.Id driverId,
            favourite = favourite,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            mobileNumber = EncryptedHashed (Encrypted mobileNumberEncrypted) mobileNumberHash,
            riderDetailId = Kernel.Types.Id.Id riderDetailId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RiderDriverCorrelation Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation where
  toTType' (Domain.Types.RiderDriverCorrelation.RiderDriverCorrelation {..}) = do
    Beam.RiderDriverCorrelationT
      { Beam.createdAt = createdAt,
        Beam.driverId = Kernel.Types.Id.getId driverId,
        Beam.favourite = favourite,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.mobileNumberEncrypted = mobileNumber & unEncrypted . encrypted,
        Beam.mobileNumberHash = mobileNumber & hash,
        Beam.riderDetailId = Kernel.Types.Id.getId riderDetailId,
        Beam.updatedAt = updatedAt
      }
