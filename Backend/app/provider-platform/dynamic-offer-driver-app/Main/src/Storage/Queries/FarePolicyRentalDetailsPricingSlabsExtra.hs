{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyRentalDetailsPricingSlabsExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicyRentalDetailsPricingSlabs as DTFPRDPS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyRentalDetailsPricingSlabs as BeamFPRDPS
import Storage.Queries.OrphanInstances.FarePolicyRentalDetailsPricingSlabs

type FullFarePolicyRentalDetailsPricingSlabs = (KTI.Id DFP.FarePolicy, DFP.FPRentalDetailsPricingSlabs)

toDomainType :: FullFarePolicyRentalDetailsPricingSlabs -> DTFPRDPS.FarePolicyRentalDetailsPricingSlabs
toDomainType (KTI.Id farePolicyId, DFP.FPRentalDetailsPricingSlabs {..}) =
  DTFPRDPS.FarePolicyRentalDetailsPricingSlabs {..}

toFullType :: DTFPRDPS.FarePolicyRentalDetailsPricingSlabs -> FullFarePolicyRentalDetailsPricingSlabs
toFullType DTFPRDPS.FarePolicyRentalDetailsPricingSlabs {..} =
  (KTI.Id farePolicyId, DFP.FPRentalDetailsPricingSlabs {..})

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullFarePolicyRentalDetailsPricingSlabs]
findAll' farePolicyId = do
  results <- findAllWithOptionsKV [Se.Is BeamFPRDPS.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPRDPS.distancePercentage) Nothing Nothing
  pure $ toFullType <$> results

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPRDPS.farePolicyId $ Se.Eq (getId farePolicyId)]
