{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MerchantOperatingCity where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Beckn.Context
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MerchantOperatingCity as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.MerchantOperatingCity.MerchantOperatingCity] -> m ()
createMany = traverse_ createWithKV

findById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe (Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findById (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.Is Beam.id $ Se.Eq id
    ]

findByMerchantIdAndCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe (Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantIdAndCity (Kernel.Types.Id.Id merchantId) city = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantId $ Se.Eq merchantId,
          Se.Is Beam.city $ Se.Eq city
        ]
    ]

findByMerchantShortIdAndCity :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.ShortId Domain.Types.Merchant.Merchant -> Kernel.Types.Beckn.Context.City -> m (Maybe (Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByMerchantShortIdAndCity (Kernel.Types.Id.ShortId merchantShortId) city = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantShortId $ Se.Eq merchantShortId,
          Se.Is Beam.city $ Se.Eq city
        ]
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Maybe (Domain.Types.MerchantOperatingCity.MerchantOperatingCity))
findByPrimaryKey (Kernel.Types.Id.Id id) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m ()
updateByPrimaryKey Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.city $ city,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantShortId $ (Kernel.Types.Id.getShortId merchantShortId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)
        ]
    ]

instance FromTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  fromTType' Beam.MerchantOperatingCityT {..} = do
    pure $
      Just
        Domain.Types.MerchantOperatingCity.MerchantOperatingCity
          { city = city,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantShortId = Kernel.Types.Id.ShortId merchantShortId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantOperatingCity Domain.Types.MerchantOperatingCity.MerchantOperatingCity where
  toTType' Domain.Types.MerchantOperatingCity.MerchantOperatingCity {..} = do
    Beam.MerchantOperatingCityT
      { Beam.city = city,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantShortId = Kernel.Types.Id.getShortId merchantShortId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
