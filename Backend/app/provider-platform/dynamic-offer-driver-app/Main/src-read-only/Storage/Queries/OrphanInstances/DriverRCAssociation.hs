{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.DriverRCAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DriverRCAssociation
import qualified Storage.Beam.DriverRCAssociation as Beam
import qualified Kernel.Types.Id



instance FromTType' Beam.DriverRCAssociation Domain.Types.DriverRCAssociation.DriverRCAssociation
    where fromTType' (Beam.DriverRCAssociationT {..}) = do pure $ Just Domain.Types.DriverRCAssociation.DriverRCAssociation{associatedOn = associatedOn,
                                                                                                                            associatedTill = associatedTill,
                                                                                                                            consent = consent,
                                                                                                                            consentTimestamp = consentTimestamp,
                                                                                                                            driverId = Kernel.Types.Id.Id driverId,
                                                                                                                            errorMessage = errorMessage,
                                                                                                                            id = Kernel.Types.Id.Id id,
                                                                                                                            isRcActive = isRcActive,
                                                                                                                            rcId = Kernel.Types.Id.Id rcId,
                                                                                                                            merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                                            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                                            createdAt = createdAt,
                                                                                                                            updatedAt = updatedAt}
instance ToTType' Beam.DriverRCAssociation Domain.Types.DriverRCAssociation.DriverRCAssociation
    where toTType' (Domain.Types.DriverRCAssociation.DriverRCAssociation {..}) = do Beam.DriverRCAssociationT{Beam.associatedOn = associatedOn,
                                                                                                              Beam.associatedTill = associatedTill,
                                                                                                              Beam.consent = consent,
                                                                                                              Beam.consentTimestamp = consentTimestamp,
                                                                                                              Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                              Beam.errorMessage = errorMessage,
                                                                                                              Beam.id = Kernel.Types.Id.getId id,
                                                                                                              Beam.isRcActive = isRcActive,
                                                                                                              Beam.rcId = Kernel.Types.Id.getId rcId,
                                                                                                              Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                                              Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                                              Beam.createdAt = createdAt,
                                                                                                              Beam.updatedAt = updatedAt}



