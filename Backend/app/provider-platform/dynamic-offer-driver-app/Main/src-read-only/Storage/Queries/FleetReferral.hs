{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FleetReferral (module Storage.Queries.FleetReferral, module ReExport) where

import qualified Data.Text
import qualified Domain.Types.FleetReferral
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FleetReferral as Beam
import Storage.Queries.FleetReferralExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetReferral.FleetReferral -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FleetReferral.FleetReferral] -> m ())
createMany = traverse_ create

findById :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Data.Text.Text -> m (Maybe Domain.Types.FleetReferral.FleetReferral))
findById fleetOwnerId = do findOneWithKV [Se.Is Beam.fleetOwnerId $ Se.Eq fleetOwnerId]

findByReferralCode :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetReferral.FleetReferral -> m (Maybe Domain.Types.FleetReferral.FleetReferral))
findByReferralCode referralCode = do findOneWithKV [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]

findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.FleetReferral.FleetReferral -> m (Maybe Domain.Types.FleetReferral.FleetReferral))
findByPrimaryKey referralCode = do findOneWithKV [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FleetReferral.FleetReferral -> m ())
updateByPrimaryKey (Domain.Types.FleetReferral.FleetReferral {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.fleetOwnerId fleetOwnerId,
      Se.Set Beam.linkedAt linkedAt,
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.referralCode $ Se.Eq (Kernel.Types.Id.getId referralCode)]]
