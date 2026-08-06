{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantAccess where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantAccess
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantAccess as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantAccess.MerchantAccess -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MerchantAccess.MerchantAccess] -> m ())
createMany = traverse_ create

deleteAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
deleteAllByPersonId personId = do deleteWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

deleteAllByPersonIdAndMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ())
deleteAllByPersonIdAndMerchantId personId merchantId = do
  deleteWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantAccess.MerchantAccess -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.MerchantAccess.MerchantAccess]))
findAllByMerchantId merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.MerchantAccess.MerchantAccess]))
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findAllUserAccountForMerchant :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.MerchantAccess.MerchantAccess]))
findAllUserAccountForMerchant merchantId = do findAllWithKV [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantAccess.MerchantAccess -> m (Maybe Domain.Types.MerchantAccess.MerchantAccess))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPersonIdAndMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.MerchantAccess.MerchantAccess]))
findByPersonIdAndMerchantId personId merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findByPersonIdAndMerchantIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe Domain.Types.MerchantAccess.MerchantAccess))
findByPersonIdAndMerchantIdAndCity personId merchantId operatingCity = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.operatingCity $ Se.Eq operatingCity
        ]
    ]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.MerchantAccess.MerchantAccess -> m (Maybe Domain.Types.MerchantAccess.MerchantAccess))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MerchantAccess.MerchantAccess -> m ())
updateByPrimaryKey (Domain.Types.MerchantAccess.MerchantAccess {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.is2faEnabled is2faEnabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantShortId (Kernel.Types.Id.getShortId merchantShortId),
      Se.Set Beam.operatingCity operatingCity,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.secretKey secretKey,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.MerchantAccess Domain.Types.MerchantAccess.MerchantAccess where
  fromTType' (Beam.MerchantAccessT {..}) = do
    pure $
      Just
        Domain.Types.MerchantAccess.MerchantAccess
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            is2faEnabled = is2faEnabled,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            operatingCity = operatingCity,
            personId = Kernel.Types.Id.Id personId,
            secretKey = secretKey,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantAccess Domain.Types.MerchantAccess.MerchantAccess where
  toTType' (Domain.Types.MerchantAccess.MerchantAccess {..}) = do
    Beam.MerchantAccessT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.is2faEnabled = is2faEnabled,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.operatingCity = operatingCity,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.secretKey = secretKey,
        Beam.updatedAt = updatedAt
      }
