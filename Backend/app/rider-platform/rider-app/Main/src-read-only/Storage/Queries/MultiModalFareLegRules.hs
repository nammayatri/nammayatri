{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.MultiModalFareLegRules (module Storage.Queries.MultiModalFareLegRules, module ReExport) where

import qualified Domain.Types.MultiModalFareLegRules
import qualified Domain.Types.MultiModalNetwork
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.MultiModalFareLegRules as Beam
import Storage.Queries.MultiModalFareLegRulesExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules] -> m ())
createMany = traverse_ create

findById ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules -> m (Maybe Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules))
findById id = do findOneWithKV [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]

findByMaxDist :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.Meters -> m ([Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules]))
findByMaxDist maxDist = do findAllWithKV [Se.Is Beam.maxDist $ Se.Eq maxDist]

findByMinDist :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Common.Meters -> m ([Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules]))
findByMinDist minDist = do findAllWithKV [Se.Is Beam.minDist $ Se.Eq minDist]

findByNetworkId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalNetwork.MultiModalNetwork -> m ([Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules]))
findByNetworkId networkId = do findAllWithKV [Se.Is Beam.networkId $ Se.Eq (Kernel.Types.Id.getId networkId)]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules -> m (Maybe Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules))
findByPrimaryKey id = do findOneWithKV [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules -> m ())
updateByPrimaryKey (Domain.Types.MultiModalFareLegRules.MultiModalFareLegRules {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.amount amount,
      Se.Set Beam.currency currency,
      Se.Set Beam.fromTimeFrameId (Kernel.Types.Id.getId fromTimeFrameId),
      Se.Set Beam.maxDist maxDist,
      Se.Set Beam.minDist minDist,
      Se.Set Beam.networkId (Kernel.Types.Id.getId networkId),
      Se.Set Beam.passengerType passengerType,
      Se.Set Beam.paymentMedia paymentMedia,
      Se.Set Beam.toTimeFrameId (Kernel.Types.Id.getId toTimeFrameId),
      Se.Set Beam.merchantId (Kernel.Types.Id.getId <$> merchantId),
      Se.Set Beam.merchantOperatingCityId (Kernel.Types.Id.getId <$> merchantOperatingCityId),
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Kernel.Types.Id.getId id)]]
