{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.AutoCompleteData where

import qualified Domain.Types.AutoCompleteData
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.AutoCompleteData as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => Domain.Types.AutoCompleteData.AutoCompleteData -> m ()
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => [Domain.Types.AutoCompleteData.AutoCompleteData] -> m ()
createMany = traverse_ createWithKV

findBySessionTokenAndSearchType :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Text -> m (Maybe (Domain.Types.AutoCompleteData.AutoCompleteData))
findBySessionTokenAndSearchType sessionToken searchType = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.sessionToken $ Se.Eq $ sessionToken,
          Se.Is Beam.searchType $ Se.Eq $ searchType
        ]
    ]

updateInputById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> Kernel.Prelude.Text -> m ()
updateInputById autocompleteInputs id = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.autocompleteInputs $ autocompleteInputs,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq $ id
    ]

updateSearchRequestIdAndisLocationSelectedOnMapById :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest) -> Kernel.Prelude.Maybe Kernel.Prelude.Bool -> Kernel.Prelude.Text -> m ()
updateSearchRequestIdAndisLocationSelectedOnMapById searchRequestId isLocationSelectedOnMap id = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.searchRequestId $ (Kernel.Types.Id.getId <$> searchRequestId),
      Se.Set Beam.isLocationSelectedOnMap $ isLocationSelectedOnMap,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.Is Beam.id $ Se.Eq $ id
    ]

findByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Kernel.Prelude.Text -> m (Maybe (Domain.Types.AutoCompleteData.AutoCompleteData))
findByPrimaryKey id = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ id
        ]
    ]

updateByPrimaryKey :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Domain.Types.AutoCompleteData.AutoCompleteData -> m ()
updateByPrimaryKey Domain.Types.AutoCompleteData.AutoCompleteData {..} = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.autocompleteInputs $ autocompleteInputs,
      Se.Set Beam.customerId $ (Kernel.Types.Id.getId customerId),
      Se.Set Beam.isLocationSelectedOnMap $ isLocationSelectedOnMap,
      Se.Set Beam.searchRequestId $ (Kernel.Types.Id.getId <$> searchRequestId),
      Se.Set Beam.searchType $ searchType,
      Se.Set Beam.sessionToken $ sessionToken,
      Se.Set Beam.merchantId $ (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId $ (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt $ createdAt,
      Se.Set Beam.updatedAt $ now
    ]
    [ Se.And
        [ Se.Is Beam.id $ Se.Eq $ id
        ]
    ]

instance FromTType' Beam.AutoCompleteData Domain.Types.AutoCompleteData.AutoCompleteData where
  fromTType' Beam.AutoCompleteDataT {..} = do
    pure $
      Just
        Domain.Types.AutoCompleteData.AutoCompleteData
          { autocompleteInputs = autocompleteInputs,
            customerId = Kernel.Types.Id.Id customerId,
            id = id,
            isLocationSelectedOnMap = isLocationSelectedOnMap,
            searchRequestId = Kernel.Types.Id.Id <$> searchRequestId,
            searchType = searchType,
            sessionToken = sessionToken,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.AutoCompleteData Domain.Types.AutoCompleteData.AutoCompleteData where
  toTType' Domain.Types.AutoCompleteData.AutoCompleteData {..} = do
    Beam.AutoCompleteDataT
      { Beam.autocompleteInputs = autocompleteInputs,
        Beam.customerId = Kernel.Types.Id.getId customerId,
        Beam.id = id,
        Beam.isLocationSelectedOnMap = isLocationSelectedOnMap,
        Beam.searchRequestId = Kernel.Types.Id.getId <$> searchRequestId,
        Beam.searchType = searchType,
        Beam.sessionToken = sessionToken,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
