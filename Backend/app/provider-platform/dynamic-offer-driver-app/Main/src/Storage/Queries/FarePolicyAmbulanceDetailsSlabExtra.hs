{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyAmbulanceDetailsSlabExtra where

import qualified Domain.Types.FarePolicy as Domain
import qualified Domain.Types.FarePolicy.FarePolicyAmbulanceDetails as FPASlab
import qualified Domain.Types.FarePolicyAmbulanceDetailsSlab as DTFPADS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyAmbulanceDetailsSlab as BeamFPAD
import Storage.Queries.OrphanInstances.FarePolicyAmbulanceDetailsSlab

type FullFarePolicyAmbulanceDetailsSlab = (KTI.Id Domain.FarePolicy, Domain.FPAmbulanceDetailsSlab)

toDomainType :: FullFarePolicyAmbulanceDetailsSlab -> DTFPADS.FarePolicyAmbulanceDetailsSlab
toDomainType (KTI.Id farePolicyId, Domain.FPAmbulanceDetailsSlab {..}) =
  DTFPADS.FarePolicyAmbulanceDetailsSlab {..}

toFullType :: DTFPADS.FarePolicyAmbulanceDetailsSlab -> FullFarePolicyAmbulanceDetailsSlab
toFullType DTFPADS.FarePolicyAmbulanceDetailsSlab {..} =
  (KTI.Id farePolicyId, Domain.FPAmbulanceDetailsSlab {..})

findById' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m [FullFarePolicyAmbulanceDetailsSlab]
findById' (KTI.Id farePolicyId') = do
  results <- findAllWithOptionsKV [Se.Is BeamFPAD.farePolicyId $ Se.Eq farePolicyId'] (Se.Asc BeamFPAD.vehicleAge) Nothing Nothing
  pure $ toFullType <$> results

getNextSlabId :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => m Int
getNextSlabId = do
  rows <- findAllWithOptionsKV [Se.Is BeamFPAD.id $ Se.GreaterThanOrEq 0] (Se.Desc BeamFPAD.id) (Just 1) Nothing
  pure $ case listToMaybe rows of
    Just slab -> DTFPADS.id slab + 1
    Nothing -> 1

create :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FullFarePolicyAmbulanceDetailsSlab -> m ()
create (farePolicyId, slab) = do
  nextId <- getNextSlabId
  createWithKV $ toDomainType (farePolicyId, slab {FPASlab.id = nextId})

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => KTI.Id Domain.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPAD.farePolicyId $ Se.Eq (KTI.getId farePolicyId)]
