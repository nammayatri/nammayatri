{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSection (module Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSection, module ReExport) where

import qualified Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection as Beam
import Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSectionExtra as ReExport

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection] -> m ())
createMany = traverse_ create

deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Prelude.Text -> m [Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection])
findAllByFarePolicyId limit offset farePolicyId = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.startDistance) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> m (Maybe Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection))
findByPrimaryKey farePolicyId = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection -> m ())
updateByPrimaryKey (Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection {..}) = do
  updateWithKV
    [ Se.Set Beam.baseFareDepreciation (Kernel.Prelude.Just baseFareDepreciation),
      Se.Set Beam.distanceUnit (Kernel.Prelude.Just distanceUnit),
      Se.Set Beam.perExtraKmRate perExtraKmRate,
      Se.Set Beam.startDistance startDistance
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]
