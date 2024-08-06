{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FullFarePolicyProgressiveDetailsPerMinRateSection where

import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FullFarePolicyProgressiveDetailsPerMinRateSection as Beam

create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection -> m ())
create = createWithKV

createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection] -> m ())
createMany = traverse_ create

deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

findAllByFarePolicyId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Prelude.Text -> m [Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection])
findAllByFarePolicyId farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]

instance FromTType' Beam.FullFarePolicyProgressiveDetailsPerMinRateSection Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection where
  fromTType' (Beam.FullFarePolicyProgressiveDetailsPerMinRateSectionT {..}) = do
    pure $
      Just
        Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection
          { currency = currency,
            farePolicyId = farePolicyId,
            perMinRate = perMinRate,
            rideDuration = Kernel.Types.Common.Minutes rideDurationInMin,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.FullFarePolicyProgressiveDetailsPerMinRateSection Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection where
  toTType' (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection {..}) = do
    Beam.FullFarePolicyProgressiveDetailsPerMinRateSectionT
      { Beam.currency = currency,
        Beam.farePolicyId = farePolicyId,
        Beam.perMinRate = perMinRate,
        Beam.rideDurationInMin = rideDuration & (.getMinutes),
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
