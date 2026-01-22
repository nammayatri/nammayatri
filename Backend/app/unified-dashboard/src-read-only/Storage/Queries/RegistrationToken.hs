{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.RegistrationToken where

import qualified Data.Text
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RegistrationToken
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.RegistrationToken as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistrationToken.RegistrationToken -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.RegistrationToken.RegistrationToken] -> m ())
createMany = traverse_ create

deleteById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m ())
deleteById id = do deleteWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findAllByMerchantIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m ([Domain.Types.RegistrationToken.RegistrationToken]))
findAllByMerchantIdAndCity merchantId operatingCity = do findAllWithKV [Se.And [Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId), Se.Is Beam.operatingCity $ Se.Eq operatingCity]]

findAllByPersonId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m ([Domain.Types.RegistrationToken.RegistrationToken]))
findAllByPersonId personId = do findAllWithKV [Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId)]

findAllByPersonIdAndMerchantId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> m ([Domain.Types.RegistrationToken.RegistrationToken]))
findAllByPersonIdAndMerchantId personId merchantId = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId)
        ]
    ]

findAllByPersonIdAndMerchantIdAndCity ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Person.Person -> Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m ([Domain.Types.RegistrationToken.RegistrationToken]))
findAllByPersonIdAndMerchantIdAndCity personId merchantId operatingCity = do
  findAllWithKV
    [ Se.And
        [ Se.Is Beam.personId $ Se.Eq (Kernel.Types.Id.getId personId),
          Se.Is Beam.merchantId $ Se.Eq (Kernel.Types.Id.getId merchantId),
          Se.Is Beam.operatingCity $ Se.Eq operatingCity
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m (Maybe Domain.Types.RegistrationToken.RegistrationToken))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByToken :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.RegistrationToken.RegistrationToken))
findByToken token = do findOneWithKV [Se.Is Beam.token $ Se.Eq token]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.RegistrationToken.RegistrationToken -> m (Maybe Domain.Types.RegistrationToken.RegistrationToken))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.RegistrationToken.RegistrationToken -> m ())
updateByPrimaryKey (Domain.Types.RegistrationToken.RegistrationToken {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.enabled enabled,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.operatingCity operatingCity,
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.token token,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

instance FromTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  fromTType' (Beam.RegistrationTokenT {..}) = do
    pure $
      Just
        Domain.Types.RegistrationToken.RegistrationToken
          { createdAt = createdAt,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            operatingCity = operatingCity,
            personId = Kernel.Types.Id.Id personId,
            token = token,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            updatedAt = updatedAt
          }

instance ToTType' Beam.RegistrationToken Domain.Types.RegistrationToken.RegistrationToken where
  toTType' (Domain.Types.RegistrationToken.RegistrationToken {..}) = do
    Beam.RegistrationTokenT
      { Beam.createdAt = createdAt,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.operatingCity = operatingCity,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.token = token,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.updatedAt = updatedAt
      }
