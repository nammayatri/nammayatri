{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.FarePolicyInterCityDetailsPricingSlabsExtra where

import qualified Domain.Types.FarePolicy as DFP
import qualified Domain.Types.FarePolicy.FarePolicyInterCityDetailsPricingSlabs as DFP
import qualified Domain.Types.FarePolicyInterCityDetailsPricingSlabs as DTFPICDPS
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id as KTI
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FarePolicyInterCityDetailsPricingSlabs as BeamFPICDPS
import Storage.Queries.OrphanInstances.FarePolicyInterCityDetailsPricingSlabs

type FullFarePolicyInterCityDetailsPricingSlabs = (KTI.Id DFP.FarePolicy, DFP.FPInterCityDetailsPricingSlabs)

toDomainType :: FullFarePolicyInterCityDetailsPricingSlabs -> DTFPICDPS.FarePolicyInterCityDetailsPricingSlabs
toDomainType (KTI.Id farePolicyId, DFP.FPInterCityDetailsPricingSlabs {..}) =
  DTFPICDPS.FarePolicyInterCityDetailsPricingSlabs {..}

toFullType :: DTFPICDPS.FarePolicyInterCityDetailsPricingSlabs -> FullFarePolicyInterCityDetailsPricingSlabs
toFullType DTFPICDPS.FarePolicyInterCityDetailsPricingSlabs {..} =
  (KTI.Id farePolicyId, DFP.FPInterCityDetailsPricingSlabs {..})

findAll' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m [FullFarePolicyInterCityDetailsPricingSlabs]
findAll' farePolicyId = do
  results <- findAllWithOptionsKV [Se.Is BeamFPICDPS.farePolicyId $ Se.Eq (getId farePolicyId)] (Se.Asc BeamFPICDPS.distancePercentage) Nothing Nothing
  pure $ toFullType <$> results

delete :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id DFP.FarePolicy -> m ()
delete farePolicyId = deleteWithKV [Se.Is BeamFPICDPS.farePolicyId $ Se.Eq (getId farePolicyId)]
