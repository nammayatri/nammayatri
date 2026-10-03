{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyProgressiveDetailsPerExtraKmRateSectionExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection as DTFPPDEKRS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection as BeamFPPDP
import Storage.Queries.OrphanInstances.FarePolicyProgressiveDetailsPerExtraKmRateSection

type FullFarePolicyProgressiveDetailsPerExtraKmRateSection = (KTI.Id DFP.FarePolicy, DFP.FPProgressiveDetailsPerExtraKmRateSection)

toDomainType :: FullFarePolicyProgressiveDetailsPerExtraKmRateSection -> DTFPPDEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection
toDomainType (KTI.Id farePolicyId, DFP.FPProgressiveDetailsPerExtraKmRateSection {..}) =
  DTFPPDEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection {..}

toFullType :: DTFPPDEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection -> FullFarePolicyProgressiveDetailsPerExtraKmRateSection
toFullType DTFPPDEKRS.FarePolicyProgressiveDetailsPerExtraKmRateSection {..} =
  (KTI.Id farePolicyId, DFP.FPProgressiveDetailsPerExtraKmRateSection {..})

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullFarePolicyProgressiveDetailsPerExtraKmRateSection]
findAll' farePolicyId = do
  results <- findAllWithOptionsKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPPDP.startDistance) Nothing Nothing
  pure $ toFullType <$> results

findByIdAndStartDistance :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id DFP.FarePolicy -> Meters -> m (Maybe FullFarePolicyProgressiveDetailsPerExtraKmRateSection)
findByIdAndStartDistance farePolicyId' startDistance = do
  result <- findOneWithKV [Se.And [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId'), Se.Is BeamFPPDP.startDistance $ Se.Eq startDistance]]
  pure $ toFullType <$> result

updatePerExtraKmRate :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id DFP.FarePolicy -> Meters -> HighPrecMoney -> m ()
updatePerExtraKmRate farePolicyId' startDistance perExtraKmRate =
  updateWithKV
    [Se.Set BeamFPPDP.perExtraKmRate perExtraKmRate]
    [Se.And [Se.Is BeamFPPDP.farePolicyId $ Se.Eq (getId farePolicyId'), Se.Is BeamFPPDP.startDistance $ Se.Eq startDistance]]

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
deleteAll' (Id farePolicyId) = deleteWithKV [Se.Is BeamFPPDP.farePolicyId $ Se.Eq farePolicyId]
