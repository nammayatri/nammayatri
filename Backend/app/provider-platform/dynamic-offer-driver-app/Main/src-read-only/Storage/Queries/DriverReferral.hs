{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverReferral (module Storage.Queries.DriverReferral, module ReExport) where

import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.DriverReferral as Beam
import Storage.Queries.DriverReferralExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverReferral.DriverReferral -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.DriverReferral.DriverReferral] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findById driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByRefferalCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByRefferalCode referralCode = do findOneWithKV [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByPrimaryKey referralCode = do findOneWithKV [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverReferral.DriverReferral -> m ())
updateByPrimaryKey (Domain.Types.DriverReferral.DriverReferral {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.linkedAt linkedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]
