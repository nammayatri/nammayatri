{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FullFarePolicyProgressiveDetailsPerExtraMinRateSection where

import qualified Domain.Types.FarePolicy
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSection as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection] -> m ())
createMany = traverse_ create

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Maybe Int -> Maybe Int -> Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy -> m ([Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection]))
findAllByFarePolicyId limit offset (Kernel.Types.Id.Id farePolicyId) = do findAllWithOptionsKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId] (Se.Asc Beam.startMin) limit offset

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.FarePolicy.FarePolicy -> m (Maybe Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection))
findByPrimaryKey (Kernel.Types.Id.Id farePolicyId) = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]]

updateByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection -> m ())
updateByPrimaryKey (Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.perExtraMinRate perExtraMinRate,
      Se.Set Beam.startMin startMin,
      Se.Set Beam.createdAt createdAt,
      Se.Set Beam.updatedAt _now
    ]
    [Se.And [Se.Is Beam.farePolicyId $ Se.Eq (Kernel.Types.Id.getId farePolicyId)]]

instance
  FromTType'
    Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSection
    Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection
  where
  fromTType' (Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT {..}) = do
    pure $
      Just
        Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection
          { farePolicyId = Kernel.Types.Id.Id farePolicyId,
            perExtraMinRate = perExtraMinRate,
            startMin = startMin,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSection Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection where
  toTType' (Domain.Types.FullFarePolicyProgressiveDetailsPerExtraMinRateSection.FullFarePolicyProgressiveDetailsPerExtraMinRateSection {..}) = do
    Beam.FullFarePolicyProgressiveDetailsPerExtraMinRateSectionT
      { Beam.farePolicyId = Kernel.Types.Id.getId farePolicyId,
        Beam.perExtraMinRate = perExtraMinRate,
        Beam.startMin = startMin,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
