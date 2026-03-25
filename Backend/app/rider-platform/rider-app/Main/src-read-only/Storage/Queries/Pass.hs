{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.Pass (module Storage.Queries.Pass, module ReExport) where

import qualified Domain.Types.Pass
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.Pass as Beam
import Storage.Queries.PassExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.Pass.Pass] -> m ())
createMany = traverse_ create

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Pass.Pass -> m (Maybe Domain.Types.Pass.Pass))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.Pass.Pass -> m ())
updateByPrimaryKey (Domain.Types.Pass.Pass {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.applicableVehicleServiceTiers applicableVehicleServiceTiers,
      Se.Set Beam.autoApply autoApply,
      Se.Set Beam.benefit benefit,
      Se.Set Beam.benefitDescription benefitDescription,
      Se.Set Beam.code code,
      Se.Set Beam.description description,
      Se.Set Beam.documentsRequired documentsRequired,
      Se.Set Beam.enable enable,
      Se.Set Beam.maxValidDays maxValidDays,
      Se.Set Beam.maxValidTrips maxValidTrips,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId merchantOperatingCityId),
      Se.Set Beam.name name,
      Se.Set Beam.order order,
      Se.Set Beam.maxSwitchCount ((.maxSwitchCount) <$> passConfig),
      Se.Set Beam.passTypeId (Kernel.Types.Id.getId passTypeId),
      Se.Set Beam.purchaseEligibilityJsonLogic purchaseEligibilityJsonLogic,
      Se.Set Beam.redeemEligibilityJsonLogic redeemEligibilityJsonLogic,
      Se.Set Beam.verificationValidity (Kernel.Prelude.Just verificationValidity),
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
