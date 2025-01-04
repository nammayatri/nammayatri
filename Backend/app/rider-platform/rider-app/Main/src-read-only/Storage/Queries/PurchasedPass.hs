{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPass where

import qualified Domain.Types.Person
import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPass as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.PurchasedPass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PurchasedPass.PurchasedPass] -> m ())
createMany = traverse_ create

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.PurchasedPass.PurchasedPass])
findAllByPersonId limit offset personId = do findAllWithOptionsKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)] (Se.Desc Beam.createdAt) limit offset

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m (Maybe Domain.Types.PurchasedPass.PurchasedPass))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m [Domain.Types.PurchasedPass.PurchasedPass])
findByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updatePurchasedPassIdAndStatusByPersonId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> Domain.Types.PurchasedPass.StatusType -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updatePurchasedPassIdAndStatusByPersonId id status personId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.id (Kernel.Types.Id.getId id), Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateStatusByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.StatusType -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateStatusByPersonId status personId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

updateStatusByShortId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.StatusType -> Kernel.Types.Id.ShortId Domain.Types.PurchasedPass.PurchasedPass -> m ())
updateStatusByShortId status shortId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.status status, Se.Set Beam.updatedAt _now] [Se.Is Beam.shortId $ Se.Eq (Kernel.Types.Id.getShortId shortId)]

updateValidTripsLeftByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Maybe Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateValidTripsLeftByPersonId validTripsLeft personId = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.validTripsLeft validTripsLeft, Se.Set Beam.updatedAt _now] [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m (Maybe Domain.Types.PurchasedPass.PurchasedPass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.PurchasedPass -> m ())
updateByPrimaryKey (Domain.Types.PurchasedPass.PurchasedPass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.expiryDate expiryDate,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.passId (Kernel.Types.Id.getId passId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.shortId (Kernel.Types.Id.getShortId shortId),
      Se.Set Beam.status status,
      Se.Set Beam.validTripsLeft validTripsLeft,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.PurchasedPass Domain.Types.PurchasedPass.PurchasedPass where
  fromTType' (Beam.PurchasedPassT {..}) = do
    pure $
      Just
        Domain.Types.PurchasedPass.PurchasedPass
          { expiryDate = expiryDate,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            passId = Kernel.Types.Id.Id passId,
            personId = Kernel.Types.Id.Id personId,
            shortId = Kernel.Types.Id.ShortId shortId,
            status = status,
            validTripsLeft = validTripsLeft,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PurchasedPass Domain.Types.PurchasedPass.PurchasedPass where
  toTType' (Domain.Types.PurchasedPass.PurchasedPass {..}) = do
    Beam.PurchasedPassT
      { Beam.expiryDate = expiryDate,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.passId = Kernel.Types.Id.getId passId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.shortId = Kernel.Types.Id.getShortId shortId,
        Beam.status = status,
        Beam.validTripsLeft = validTripsLeft,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
