{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.DriverReferral where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DriverReferral
import qualified Storage.Beam.DriverReferral as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Prelude



instance FromTType' Beam.DriverReferral Domain.Types.DriverReferral.DriverReferral
    where fromTType' (Beam.DriverReferralT {..}) = do pure $ Just Domain.Types.DriverReferral.DriverReferral{driverId = Kernel.Types.Id.Id driverId,
                                                                                                             dynamicReferralCode = dynamicReferralCode,
                                                                                                             dynamicReferralCodeValidTill = dynamicReferralCodeValidTill,
                                                                                                             linkedAt = linkedAt,
                                                                                                             referralCode = Kernel.Types.Id.Id referralCode,
                                                                                                             role = Kernel.Prelude.fromMaybe Domain.Types.Person.DRIVER role,
                                                                                                             merchantId = Kernel.Types.Id.Id <$> merchantId,
                                                                                                             merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
                                                                                                             createdAt = createdAt,
                                                                                                             updatedAt = updatedAt}
instance ToTType' Beam.DriverReferral Domain.Types.DriverReferral.DriverReferral
    where toTType' (Domain.Types.DriverReferral.DriverReferral {..}) = do Beam.DriverReferralT{Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                               Beam.dynamicReferralCode = dynamicReferralCode,
                                                                                               Beam.dynamicReferralCodeValidTill = dynamicReferralCodeValidTill,
                                                                                               Beam.linkedAt = linkedAt,
                                                                                               Beam.referralCode = Kernel.Types.Id.getId referralCode,
                                                                                               Beam.role = Kernel.Prelude.Just role,
                                                                                               Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
                                                                                               Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
                                                                                               Beam.createdAt = createdAt,
                                                                                               Beam.updatedAt = updatedAt}



