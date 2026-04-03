{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}


module Storage.Queries.FullFarePolicyProgressiveDetailsPerMinRateSection where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection
import qualified Storage.Beam.FullFarePolicyProgressiveDetailsPerMinRateSection as Beam
import qualified Kernel.Prelude
import qualified Sequelize as Se



create :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection -> m ())
create = createWithKV
createMany :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => ([Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection] -> m ())
createMany = traverse_ create
deleteAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Text -> m ())
deleteAllByFarePolicyId farePolicyId = do deleteWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]
findAllByFarePolicyId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                         (Kernel.Prelude.Text -> m ([Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection]))
findAllByFarePolicyId farePolicyId = do findAllWithKV [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId]
findByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
                    (Kernel.Prelude.Text -> Kernel.Prelude.Int -> m (Maybe Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection))
findByPrimaryKey farePolicyId rideDurationInMin = do findOneWithKV [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId, Se.Is Beam.rideDurationInMin $ Se.Eq rideDurationInMin]]
updateByPrimaryKey :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection -> m ())
updateByPrimaryKey (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection {..}) = do {_now <- getCurrentTime;
                                                                                                                                                 updateWithKV [Se.Set Beam.currency currency,
                                                                                                                                                               Se.Set Beam.perMinRate perMinRate,
                                                                                                                                                               Se.Set Beam.updatedAt _now] [Se.And [Se.Is Beam.farePolicyId $ Se.Eq farePolicyId, Se.Is Beam.rideDurationInMin $ Se.Eq rideDurationInMin]]}



instance FromTType' Beam.FullFarePolicyProgressiveDetailsPerMinRateSection Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection
    where fromTType' (Beam.FullFarePolicyProgressiveDetailsPerMinRateSectionT {..}) = do pure $ Just Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection{currency = currency,
                                                                                                                                                                                                                      farePolicyId = farePolicyId,
                                                                                                                                                                                                                      perMinRate = perMinRate,
                                                                                                                                                                                                                      rideDurationInMin = rideDurationInMin,
                                                                                                                                                                                                                      createdAt = createdAt,
                                                                                                                                                                                                                      updatedAt = updatedAt}
instance ToTType' Beam.FullFarePolicyProgressiveDetailsPerMinRateSection Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection
    where toTType' (Domain.Types.FullFarePolicyProgressiveDetailsPerMinRateSection.FullFarePolicyProgressiveDetailsPerMinRateSection {..}) = do Beam.FullFarePolicyProgressiveDetailsPerMinRateSectionT{Beam.currency = currency,
                                                                                                                                                                                                        Beam.farePolicyId = farePolicyId,
                                                                                                                                                                                                        Beam.perMinRate = perMinRate,
                                                                                                                                                                                                        Beam.rideDurationInMin = rideDurationInMin,
                                                                                                                                                                                                        Beam.createdAt = createdAt,
                                                                                                                                                                                                        Beam.updatedAt = updatedAt}



