{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.DriverReferral (module Storage.Queries.DriverReferral, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.DriverReferral
import qualified Domain.Types.Merchant
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.Person
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
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

findByDynamicReferralCodeAndMerchantCityIdAndDriversIn ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe Data.Text.Text -> [Kernel.Types.Id.Id Domain.Types.Person.Person] -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByDynamicReferralCodeAndMerchantCityIdAndDriversIn merchantOperatingCityId dynamicReferralCode driverIds = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.dynamicReferralCode $ Se.Eq dynamicReferralCode,
          Se.Is Beam.driverId $ Se.In (Kernel.Types.Id.getId <$> driverIds)
        ]
    ]

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Person.Person -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findById driverId = do findOneWithKV [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByReferralCodeAndMerchantCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByReferralCodeAndMerchantCityId merchantOperatingCityId referralCode = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.merchantOperatingCityId $ Se.Eq (Kernel.Types.Id.getId <$> merchantOperatingCityId),
          Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)
        ]
    ]

findByRefferalCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByRefferalCode referralCode = do findOneWithKV [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]

updateDynamicReferralCode ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe Data.Text.Text -> Kernel.Prelude.Maybe Kernel.Prelude.UTCTime -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateDynamicReferralCode dynamicReferralCode dynamicReferralCodeValidTill merchantOperatingCityId merchantId driverId = do
  _now <- getCurrentTime
  updateOneWithKV
    [ Se.Set Beam.dynamicReferralCode dynamicReferralCode,
      Se.Set Beam.dynamicReferralCodeValidTill dynamicReferralCodeValidTill,
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

updateMerchantIdAndCityIdByDriverId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant) -> Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity) -> Kernel.Types.Id.Id Domain.Types.Person.Person -> m ())
updateMerchantIdAndCityIdByDriverId merchantId merchantOperatingCityId driverId = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.Is Beam.driverId $ Se.Eq (Kernel.Types.Id.getId driverId)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.DriverReferral.DriverReferral -> m (Maybe Domain.Types.DriverReferral.DriverReferral))
findByPrimaryKey referralCode = do findOneWithKV [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.DriverReferral.DriverReferral -> m ())
updateByPrimaryKey (Domain.Types.DriverReferral.DriverReferral {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.driverId (Kernel.Types.Id.getId driverId),
      Se.Set Beam.dynamicReferralCode dynamicReferralCode,
      Se.Set Beam.dynamicReferralCodeValidTill dynamicReferralCodeValidTill,
      Se.Set Beam.linkedAt linkedAt,
      Se.Set Beam.role (Kernel.Prelude.Just role),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]
