{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FullFarePolicyInterCityDetailsPerKmRateSection where

import qualified Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FullFarePolicyInterCityDetailsPerKmRateSection as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection] -> m ())
createMany = traverse_ create

deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> m ([Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection]))
findAllByFarePolicyId farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findByPrimaryKey ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> Kernel.Prelude.Bool -> Kernel.Types.Common.Meters -> m (Maybe Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection))
findByPrimaryKey farePolicyId isRoundTrip startDistance = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq farePolicyId,
          Se.Is Beam.isRoundTrip $ Se.Eq isRoundTrip,
          Se.Is Beam.startDistance $ Se.Eq startDistance
        ]
    ]

updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection -> m ())
updateByPrimaryKey (Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection {..}) = do
  _now <- getCurrentTime
  updateWithKV
    [Se.Set Beam.perKmRate perKmRate, Se.Set Beam.updatedAt _now]
    [ Se.And
        [ Se.Is Beam.farePolicyId $ Se.Eq farePolicyId,
          Se.Is Beam.isRoundTrip $ Se.Eq isRoundTrip,
          Se.Is Beam.startDistance $ Se.Eq startDistance
        ]
    ]

instance FromTType' Beam.FullFarePolicyInterCityDetailsPerKmRateSection Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection where
  fromTType' (Beam.FullFarePolicyInterCityDetailsPerKmRateSectionT {..}) = do
    pure $
      Just
        Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection
          { farePolicyId = farePolicyId,
            isRoundTrip = isRoundTrip,
            perKmRate = perKmRate,
            startDistance = startDistance,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FullFarePolicyInterCityDetailsPerKmRateSection Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection where
  toTType' (Domain.Types.FullFarePolicyInterCityDetailsPerKmRateSection.FullFarePolicyInterCityDetailsPerKmRateSection {..}) = do
    Beam.FullFarePolicyInterCityDetailsPerKmRateSectionT
      { Beam.farePolicyId = farePolicyId,
        Beam.isRoundTrip = isRoundTrip,
        Beam.perKmRate = perKmRate,
        Beam.startDistance = startDistance,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
