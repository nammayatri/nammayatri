{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyInterCityDetailsPricingSlabs (module Storage.Queries.FarePolicyInterCityDetailsPricingSlabs, module ReExport) where

import qualified Domain.Types.FarePolicyInterCityDetailsPricingSlabs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyInterCityDetailsPricingSlabs as Beam
import Storage.Queries.FarePolicyInterCityDetailsPricingSlabsExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs] -> m ())
createMany = traverse_ create

deleteByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.distancePercentage) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Int -> Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Maybe Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs))
findByPrimaryKey distancePercentage farePolicyId timePercentage = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.distancePercentage $ Se.Eq distancePercentage,
          Se.Is Beam.farePolicyId $ Se.Eq farePolicyId,
          Se.Is Beam.timePercentage $ Se.Eq timePercentage
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs {..}) = do
  updateWithKV
    [ Se.Set Beam.farePercentage farePercentage,
      Se.Set Beam.includeActualDistPercentage includeActualDistPercentage,
      Se.Set Beam.includeActualTimePercentage includeActualTimePercentage
    ]
    [ Se.And
        [ Se.Is Beam.distancePercentage $ Se.Eq distancePercentage,
          Se.Is Beam.farePolicyId $ Se.Eq farePolicyId,
          Se.Is Beam.timePercentage $ Se.Eq timePercentage
        ]
    ]
