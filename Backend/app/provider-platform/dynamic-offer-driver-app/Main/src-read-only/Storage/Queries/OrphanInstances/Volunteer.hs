{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.Volunteer where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.Volunteer
import qualified Storage.Beam.Volunteer as Beam
import qualified Kernel.Prelude
import qualified Kernel.Types.Id



instance FromTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer
    where fromTType' (Beam.VolunteerT {..}) = do pure $ Just Domain.Types.Volunteer.Volunteer{createdAt = createdAt,
                                                                                              id = Kernel.Types.Id.Id id,
                                                                                              isActive = (Kernel.Prelude.Just . Kernel.Prelude.fromMaybe True) isActive,
                                                                                              place = place,
                                                                                              updatedAt = updatedAt,
                                                                                              vendorId = Kernel.Prelude.Just vendorId,
                                                                                              merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                              merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId}
instance ToTType' Beam.Volunteer Domain.Types.Volunteer.Volunteer
    where toTType' (Domain.Types.Volunteer.Volunteer {..}) = do Beam.VolunteerT{Beam.createdAt = createdAt,
                                                                                Beam.id = Kernel.Types.Id.getId id,
                                                                                Beam.isActive = (Kernel.Prelude.Just . Kernel.Prelude.fromMaybe True) isActive,
                                                                                Beam.place = place,
                                                                                Beam.updatedAt = updatedAt,
                                                                                Beam.vendorId = Kernel.Prelude.fromMaybe "DEFAULT_VENDOR" vendorId,
                                                                                Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId}



