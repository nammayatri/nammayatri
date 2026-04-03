{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.DriverGullakAssociation where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.DriverGullakAssociation
import qualified Storage.Beam.DriverGullakAssociation as Beam
import qualified Kernel.Types.Id
import qualified Domain.Types.Person
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGullakAssociation.DriverGullakAssociation -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverGullakAssociation.DriverGullakAssociation] -> m ())
createMany = traverse_ create
updateGullakToken :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> Kernel.Prelude.UTCTime -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateGullakToken gullakToken tokenExpiry driverId = do {_now <- getCurrentTime;
                                                         updateOneWithKV [Se.Set Beam.gullakToken gullakToken, Se.Set Beam.tokenExpiry tokenExpiry, Se.Set Beam.updatedAt _now] [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]}
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverGullakAssociation.DriverGullakAssociation))
findByPrimaryKey driverId = do findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverGullakAssociation.DriverGullakAssociation -> m ())
updateByPrimaryKey (Domain.Types.DriverGullakAssociation.DriverGullakAssociation {..}) = do {_now <- getCurrentTime;
                                                                                             updateWithKV [Se.Set Beam.gullakToken gullakToken,
                                                                                                           Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
                                                                                                           Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
                                                                                                           Se.Set Beam.tokenExpiry tokenExpiry,
                                                                                                           Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]]}



instance FromTType' Beam.DriverGullakAssociation Domain.Types.DriverGullakAssociation.DriverGullakAssociation
    where fromTType' (Beam.DriverGullakAssociationT {..}) = do pure $ Just Domain.Types.DriverGullakAssociation.DriverGullakAssociation{driverId = Kernel.Types.Id.Id driverId,
                                                                                                                                        gullakToken = gullakToken,
                                                                                                                                        merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                        merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                        tokenExpiry = tokenExpiry,
                                                                                                                                        createdAt = createdAt,
                                                                                                                                        updatedAt = updatedAt}
instance ToTType' Beam.DriverGullakAssociation Domain.Types.DriverGullakAssociation.DriverGullakAssociation
    where toTType' (Domain.Types.DriverGullakAssociation.DriverGullakAssociation {..}) = do Beam.DriverGullakAssociationT{Beam.driverId = Kernel.Types.Id.getId driverId,
                                                                                                                          Beam.gullakToken = gullakToken,
                                                                                                                          Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                          Beam.tokenExpiry = tokenExpiry,
                                                                                                                          Beam.createdAt = createdAt,
                                                                                                                          Beam.updatedAt = updatedAt}



