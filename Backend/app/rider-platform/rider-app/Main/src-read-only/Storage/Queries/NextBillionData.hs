{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.NextBillionData where

import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.NextBillionData
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.NextBillionData as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.NextBillionData.NextBillionData -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.NextBillionData.NextBillionData] -> m ()
createMany = traverse_ createWithKV

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe (Domain.Types.NextBillionData.NextBillionData))
findByPrimaryKey (Kernel.Types.Id.Id searchRequestId) = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq searchRequestId
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.NextBillionData.NextBillionData -> m ()
updateByPrimaryKey Domain.Types.NextBillionData.NextBillionData {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.routes $ routes,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)
        ]
    ]

instance FromTType' Beam.NextBillionData Domain.Types.NextBillionData.NextBillionData where
  fromTType' Beam.NextBillionDataT {..} = do
    pure $
      Just
        Domain.Types.NextBillionData.NextBillionData
          { routes = routes,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.NextBillionData Domain.Types.NextBillionData.NextBillionData where
  toTType' Domain.Types.NextBillionData.NextBillionData {..} = do
    Beam.NextBillionDataT
      { Beam.routes = routes,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
