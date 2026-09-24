{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicySlabsDetailsSlabExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicySlabsDetailsSlab as DTFPSDS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicySlabsDetailsSlab as BeamFPSS
import Storage.Queries.OrphanInstances.FarePolicySlabsDetailsSlab

type FullFarePolicySlabsDetailsSlab = (KTI.Id DFP.FarePolicy, DFP.FPSlabsDetailsSlab)

toDomainType :: FullFarePolicySlabsDetailsSlab -> DTFPSDS.FarePolicySlabsDetailsSlab
toDomainType (KTI.Id farePolicyId, DFP.FPSlabsDetailsSlab {..}) =
  DTFPSDS.FarePolicySlabsDetailsSlab
    { id = Nothing,
      ..
    }

toFullType :: DTFPSDS.FarePolicySlabsDetailsSlab -> FullFarePolicySlabsDetailsSlab
toFullType DTFPSDS.FarePolicySlabsDetailsSlab {..} =
  (KTI.Id farePolicyId, DFP.FPSlabsDetailsSlab {..})

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFarePolicySlabsDetailsSlab -> m ()
create = createWithKV . toDomainType

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullFarePolicySlabsDetailsSlab]
findAll' (Id farePolicyId) = do
  results <- findAllWithOptionsKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId] (Se.Asc BeamFPSS.startDistance) Nothing Nothing
  pure $ toFullType <$> results

findById'' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m (Maybe FullFarePolicySlabsDetailsSlab)
findById'' (Id farePolicyId) = do
  results <- findAllWithKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]
  pure $ toFullType <$> listToMaybe results

deleteAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
deleteAll' (Id farePolicyId) = deleteWithKV [Se.Is BeamFPSS.farePolicyId $ Se.Eq farePolicyId]
