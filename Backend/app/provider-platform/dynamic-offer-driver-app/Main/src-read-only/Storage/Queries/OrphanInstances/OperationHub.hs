{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.OperationHub where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.OperationHub
import qualified Storage.Beam.OperationHub as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.OperationHub Domain.Types.OperationHub.OperationHub
    where fromTType' (Beam.OperationHubT {..}) = do pure $ Just Domain.Types.OperationHub.OperationHub{address = address,
                                                                                                       description = description,
                                                                                                       id = Kernel.Types.Id.Id id,
                                                                                                       lat = lat,
                                                                                                       lon = lon,
                                                                                                       merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                       merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                       mobileNumber = mobileNumber,
                                                                                                       name = name,
                                                                                                       createdAt = createdAt,
                                                                                                       updatedAt = updatedAt}
instance ToTType' Beam.OperationHub Domain.Types.OperationHub.OperationHub
    where toTType' (Domain.Types.OperationHub.OperationHub {..}) = do Beam.OperationHubT{Beam.address = address,
                                                                                         Beam.description = description,
                                                                                         Beam.id = Kernel.Types.Id.getId id,
                                                                                         Beam.lat = lat,
                                                                                         Beam.lon = lon,
                                                                                         Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                         Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                         Beam.mobileNumber = mobileNumber,
                                                                                         Beam.name = name,
                                                                                         Beam.createdAt = createdAt,
                                                                                         Beam.updatedAt = updatedAt}



