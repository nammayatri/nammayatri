{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.DriverOperatorAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DriverOperatorAssociation
import qualified Storage.Beam.DriverOperatorAssociation as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.DriverOperatorAssociation Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation
    where fromTType' (Beam.DriverOperatorAssociationT {..}) = do pure $ Just Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation{associatedOn = associatedOn,
                                                                                                                                              associatedTill = associatedTill,
                                                                                                                                              driverId = Kernel.Types.Id.Id driverId,
                                                                                                                                              id = Kernel.Types.Id.Id id,
                                                                                                                                              isActive = isActive,
                                                                                                                                              onboardingVehicleCategory = onboardingVehicleCategory,
                                                                                                                                              operatorId = operatorId,
                                                                                                                                              merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                                              merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                                                              createdAt = createdAt,
                                                                                                                                              updatedAt = updatedAt}
instance ToTType' Beam.DriverOperatorAssociation Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation
    where toTType' (Domain.Types.DriverOperatorAssociation.DriverOperatorAssociation {..}) = do Beam.DriverOperatorAssociationT{Beam.associatedOn = associatedOn,
                                                                                                                                Beam.associatedTill = associatedTill,
                                                                                                                                Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                                                Beam.id = Kernel.Types.Id.getId id,
                                                                                                                                Beam.isActive = isActive,
                                                                                                                                Beam.onboardingVehicleCategory = onboardingVehicleCategory,
                                                                                                                                Beam.operatorId = operatorId,
                                                                                                                                Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                                                Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                                                                Beam.createdAt = createdAt,
                                                                                                                                Beam.updatedAt = updatedAt}



