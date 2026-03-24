{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.ParcelDetails where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.ParcelDetails
import qualified Storage.Beam.ParcelDetails as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.SearchRequest
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.ParcelDetails.ParcelDetails -> m ())
create = createWithKV
findBySearchRequestId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.SearchRequest.SearchRequest -> m (Maybe Domain.Types.ParcelDetails.ParcelDetails))
findBySearchRequestId searchRequestId = do findOneWithKV [Se.Is Beam.searchRequestId $ Se.Eq (Kernel.Types.Id.getId searchRequestId)]



instance FromTType' Beam.ParcelDetails Domain.Types.ParcelDetails.ParcelDetails
    where fromTType' (Beam.ParcelDetailsT {..}) = do pure $ Just Domain.Types.ParcelDetails.ParcelDetails{merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                          merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                          parcelType = parcelType,
                                                                                                          quantity = quantity,
                                                                                                          searchRequestId = Kernel.Types.Id.Id searchRequestId,
                                                                                                          createdAt = createdAt,
                                                                                                          updatedAt = updatedAt}
instance ToTType' Beam.ParcelDetails Domain.Types.ParcelDetails.ParcelDetails
    where toTType' (Domain.Types.ParcelDetails.ParcelDetails {..}) = do Beam.ParcelDetailsT{Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                            Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                            Beam.parcelType = parcelType,
                                                                                            Beam.quantity = quantity,
                                                                                            Beam.searchRequestId = Kernel.Types.Id.getId searchRequestId,
                                                                                            Beam.createdAt = createdAt,
                                                                                            Beam.updatedAt = updatedAt}



