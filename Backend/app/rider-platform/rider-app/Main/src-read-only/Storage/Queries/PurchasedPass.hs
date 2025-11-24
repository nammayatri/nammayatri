{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PurchasedPass (module Storage.Queries.PurchasedPass, module ReExport) where

import qualified Domain.Types.PurchasedPass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PurchasedPass as Beam
import Storage.Queries.PurchasedPassExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.PurchasedPass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.PurchasedPass.PurchasedPass] -> m ())
createMany = traverse_ create

updateDeviceSwitchCount :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m ())
updateDeviceSwitchCount deviceSwitchCount id = do
  _now <- getCurrentTime
  updateWithKV [Se.Set Beam.deviceSwitchCount (Just deviceSwitchCount), Se.Set Beam.updatedAt _now] [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.PurchasedPass.PurchasedPass -> m (Maybe Domain.Types.PurchasedPass.PurchasedPass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.PurchasedPass.PurchasedPass -> m ())
updateByPrimaryKey (Domain.Types.PurchasedPass.PurchasedPass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.applicableVehicleServiceTiers applicableVehicleServiceTiers,
      Se.Set Beam.benefitDescription benefitDescription,
      Se.Set Beam.benefitType benefitType,
      Se.Set Beam.benefitValue benefitValue,
      Se.Set Beam.deviceId deviceId,
      Se.Set Beam.deviceSwitchCount (Just deviceSwitchCount),
      Se.Set Beam.endDate endDate,
      Se.Set Beam.maxAmount (Just maxAmount),
      Se.Set Beam.maxValidDays maxValidDays,
      Se.Set Beam.maxValidTrips maxValidTrips,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.minAmount (Just minAmount),
      Se.Set Beam.passAmount passAmount,
      Se.Set Beam.passCode passCode,
      Se.Set Beam.passDescription passDescription,
      Se.Set Beam.passName passName,
      Se.Set Beam.passNumber passNumber,
      Se.Set Beam.passTypeCode passTypeCode,
      Se.Set Beam.passTypeId (Kernel.Types.Id.getId passTypeId),
      Se.Set Beam.personId (Kernel.Types.Id.getId personId),
      Se.Set Beam.startDate startDate,
      Se.Set Beam.status status,
      Se.Set Beam.usedTripCount usedTripCount,
      Se.Set Beam.verificationValidity (Just verificationValidity),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
