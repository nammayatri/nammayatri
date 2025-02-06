{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.ParcelDetails where

import qualified Domain.Types.ParcelDetails
import qualified Domain.Types.SearchRequest
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.ParcelDetails as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ParcelDetails.ParcelDetails -> m ())
create = createWithKV

findBySearchRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe Domain.Types.ParcelDetails.ParcelDetails))
findBySearchRequestId searchRequestId = do findOneWithKV [Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)]

instance FromTType' Beam.ParcelDetails Domain.Types.ParcelDetails.ParcelDetails where
  fromTType' (Beam.ParcelDetailsT {..}) = do
    pure $
      Just
        Domain.Types.ParcelDetails.ParcelDetails
          { merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            parcelType = parcelType,
            quantity = quantity,
            searchRequestId = Kernel.Types.Id.Id searchRequestId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.ParcelDetails Domain.Types.ParcelDetails.ParcelDetails where
  toTType' (Domain.Types.ParcelDetails.ParcelDetails {..}) = do
    Beam.ParcelDetailsT
      { Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.parcelType = parcelType,
        Beam.quantity = quantity,
        Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
